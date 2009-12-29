{-

Display directory contents.

  ghc -o d MyDir.hs

-}

module Main where

import System.Exit
import System.Environment
import System.Directory
import System.FilePath
import System.Posix.Files
import System.Process
import qualified System.IO.Error as E

import System.IO
import Control.Concurrent
import Control.Monad
import qualified Control.Exception as C

import Data.Maybe (isJust)
import qualified Data.List as L

-- Having trouble compiling against split-0.1.2 so doing it manually
-- import qualified Data.List.Split as S

-- | Simple information about a given FilePath
data FileType = Directory -- ^ A directory
              | Link -- ^ A link (may be invalid)
              | Executable -- ^ Has the executable bit set for the user
              | File -- ^ None of the above
                deriving (Eq, Show)

data DirContents = DirContents {
      dirs :: [FilePath],
      links :: [FilePath],
      executables :: [FilePath],
      files :: [FilePath]
    }
                   deriving (Eq, Show)

emptyDir :: DirContents
emptyDir = DirContents [] [] [] []

-- I don't think that sorting is needed
-- since getDirectoryContents appears to return a
-- sorted list, but include for now to make sure
--
sortDC :: DirContents -> DirContents
sortDC (DirContents ds lks xs fs) = DirContents
                                    (L.sort ds)
                                    (L.sort lks)
                                    (L.sort xs)
                                    (L.sort fs)

-- | Return the type of a file, or Nothing if there
-- was an error accessing the information.
--
getFileType :: FilePath -> IO (Maybe FileType)
getFileType fp = E.catch (fmap (Just . processStatus) (getSymbolicLinkStatus fp))
                 (return . const Nothing)
    where
      -- should be re-written to be cleaner
      processStatus fs = if isDirectory fs
                            then Directory
                            else if isSymbolicLink fs
                                 then Link
                                 else if fMode == ownerExecuteMode
                                      then Executable
                                      else File
          where
            fMode = fileMode fs `intersectFileModes` ownerExecuteMode

ls :: FilePath -> IO (Either IOError [(FilePath, FileType)])
ls dname = E.try ls'
    where
      ls' = do
        fs <- getDirectoryContents dname
        ts <- mapM (getFileType . (dname </>)) fs

        let xs = filter (isJust . snd) $ zip fs ts
            ys = map (\(a,Just b) -> (a,b)) xs

        return ys

-- This step always removes "." and ".." from directories list
-- and will remove all other entries beginning with "." if 
-- flag is True
--                
lsToDir :: Bool -> [(FilePath, FileType)] -> DirContents
lsToDir flag = sortDC . foldr conv emptyDir
    where
      conv (".",Directory) inDir  = inDir
      conv ("..",Directory) inDir = inDir
      conv (fp,Directory)  inDir | flag && head fp == '.' = inDir
                                 | otherwise              = inDir { dirs = fp : dirs inDir }
      conv (fp,Link)       inDir | flag && head fp == '.' = inDir
                                 | otherwise              = inDir { links = fp : links inDir }
      conv (fp,Executable) inDir | flag && head fp == '.' = inDir
                                 | otherwise              = inDir { executables = fp : executables inDir }
      conv (fp,File)       inDir | flag && head fp == '.' = inDir
                                 | otherwise              = inDir { files = fp : files inDir }

-- | Create the column list of a set of files. Each file is limited to
-- either n or n-1 characters in length (if lastChar is Nothing
-- or Just x respectively).
--
-- If the filename is too long then the nth or n-2 th character is
-- replaced by '+'.
-- 
mkColumn :: Int -> Maybe Char -> [FilePath] -> [String]
mkColumn n lastChar = map procFileName
    where
      maxlen = if isJust lastChar then n - 1 else n
      strlen = maxlen - 1
      tooLong = '+' : flagChar
      flagChar = case lastChar of
                   Just c -> [c]
                   _      -> ""
      procFileName s = if slen > maxlen then take strlen s ++ tooLong else s ++ flagChar ++ replicate (maxlen - slen) ' '
          where
            slen = length s

-- | split at regular intervals
--
-- taken from http://www.haskell.org/haskellwiki/Data.List.Split
--
chunk :: Int -> [a] -> [[a]]
chunk _ [] = [[]]
chunk n xs = y1 : chunk n y2
  where
    (y1, y2) = splitAt n xs

-- | convert a column into a set of lines by combining
-- every n elements.
--
columnToLine :: Int -> [String] -> [String]
columnToLine n = map (L.intercalate " ") . chunk n

-- | get the number of characters in the current terminal.
-- 
terminalWidth :: IO Int
terminalWidth = do
  (_, Just outh, _, pid) <-
     createProcess (proc "stty" ["size"]) { std_in = Inherit, std_out = CreatePipe, std_err = Inherit }

  -- fork off a thread to start consuming the output
  output  <- hGetContents outh
  outMVar <- newEmptyMVar
  _ <- forkIO $ C.evaluate (length output) >> putMVar outMVar ()

  -- wait on the output
  takeMVar outMVar
  hClose outh

  -- wait on the process
  ex <- waitForProcess pid

  case ex of
    ExitSuccess   -> (return . read . tail . dropWhile (/=' ')) output
    ExitFailure r -> 
      ioError (E.mkIOError E.userErrorType ("Unable to call 'stty size' " ++
                                            " (exit " ++ show r ++ ")")
               Nothing Nothing)


listContents :: Int -- ^ terminal width in characters
             -> Bool -- ^ True if files beginning with "." should be included
             -> FilePath -- ^ directory to list
             -> IO ()
listContents tw flag dname = do
  cnts <- ls dname
  case cnts of
    Left eval   -> putStrLn ("Unable to access directory <" ++ dname ++ ">:\n" ++ show eval) >> exitFailure
    Right cnts' -> do
              let dcnts = lsToDir flag cnts'
                  ncols = 4
                  n = (tw - ncols) `div` ncols
                  dnames = mkColumn n (Just '/') (dirs dcnts)
                  xnames = mkColumn n (Just '*') (executables dcnts)
                  lnames = mkColumn n (Just '@') (links dcnts)
                  fnames = mkColumn n Nothing    (files dcnts)
              unless (null dnames) $ mapM_ putStrLn (columnToLine ncols dnames)
              unless (null xnames) $ mapM_ putStrLn (columnToLine ncols xnames)
              unless (null lnames) $ mapM_ putStrLn (columnToLine ncols lnames)
              unless (null fnames) $ mapM_ putStrLn (columnToLine ncols fnames)
              exitSuccess

usage :: IO ()
usage = getProgName >>= \n -> 
        putStrLn ("Usage: " ++ n ++ " [-a] [directory]") >> 
        exitFailure

main :: IO ()
main = do
  args <- getArgs
  tWidth <- terminalWidth
  if null args
    then listContents tWidth True "."
    else if length args > 2
       then usage
       else do
         let flag = head args == "-a"
             dname = if length args == 2 then (head . tail) args else if flag then "." else head args
         when (length args == 2 && not flag) usage
         listContents tWidth (not flag) dname
         



