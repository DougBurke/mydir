{-

Display directory contents.

The output is coloured unless either

 - this is not a TTY
 - the NO_COLOR environment variable is set to a non-empty string

-}

module Main where

import qualified Control.Exception as C
import qualified Data.List as L
import qualified System.IO.Error as E
import qualified System.Info as SI

-- import qualified PackageInfo_mydir as P
import qualified Paths_mydir as P

import Data.Maybe (fromJust, fromMaybe, isJust)

import Control.Arrow (second)
import Control.Concurrent
import Control.Monad

import Data.Version (showVersion)
import System.Console.ANSI (Color(Cyan, Magenta, Red), ColorIntensity(Dull),
                            ConsoleLayer(Foreground), SGR(SetColor),
                            hSupportsANSIColor, setSGR)
import System.Exit
import System.Environment (getArgs, getProgName, lookupEnv)
import System.Directory hiding (isSymbolicLink)
import System.FilePath
import System.IO
import System.Posix.Files
import System.Process

-- Having trouble compiling against split-0.1.2 so doing it manually
-- import qualified Data.List.Split as S

-- | Simple information about a given FilePath
data FileType = Directory -- ^ A directory
              | Link -- ^ A link (may be invalid)
              | Executable -- ^ Has the executable bit set for the user
              | BackupFile -- ^ File ends in ~ so assumed to be an emacs backup file.
                           --   This is only done for executables and files; we do
                           --   not filter out directory or links that end in ~
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
getFileType fp =
  C.catch (fmap (Just . processStatus) (getSymbolicLinkStatus fp))
          ((return . const Nothing) :: C.IOException -> IO (Maybe FileType))
    where
      processStatus fs
        | isDirectory fs            = Directory
        | isSymbolicLink fs         = Link
        | isBackup                  = BackupFile
        | fMode == ownerExecuteMode = Executable
        | otherwise                 = File
          where
            fMode = fileMode fs `intersectFileModes` ownerExecuteMode
            isBackup = "~" `L.isSuffixOf` takeFileName fp

ls :: FilePath -> IO (Either IOError [(FilePath, FileType)])
ls dname = C.try ls'
    where
      ls' = do
        fs <- getDirectoryContents dname
        ts <- mapM (getFileType . (dname </>)) fs

        let xs = filter (isJust . snd) $ zip fs ts
            ys = map (second fromJust) xs

        return ys

-- This step always removes "." and ".." from directories list
-- and will remove all other entries beginning with "." if 
-- flag is True
--
lsToDir :: Bool -> [(FilePath, FileType)] -> DirContents
lsToDir flag = sortDC . foldr conv emptyDir
    where
      conv (".", Directory) inDir          = inDir
      conv ("..", Directory) inDir         = inDir
      conv ('.':_, Directory) inDir | flag = inDir
      conv (fp, Directory) inDir           = inDir { dirs = fp : dirs inDir }

      conv ('.':_, Link) inDir | flag = inDir
      conv (fp, Link) inDir           = inDir { links = fp : links inDir }

      conv ('.':_, Executable) inDir | flag = inDir
      conv (fp, Executable) inDir           = inDir { executables = fp : executables inDir }

      conv ('.':_, File) inDir | flag = inDir
      conv (fp, File) inDir           = inDir { files = fp : files inDir }

      conv (_, BackupFile)  inDir = inDir


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
columnToLine n = map unwords . chunk n

-- | get the number of characters in the current terminal.
--
--   Could switch to System.Console.ANSI.getTerminalSize but this is
--   woking for me at the moment.
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
    ExitSuccess   -> (return . read . drop 1 . dropWhile (/=' ')) output
    ExitFailure r -> 
      C.ioError (E.mkIOError E.userErrorType ("Unable to call 'stty size' " ++
                                              " (exit " ++ show r ++ ")")
                 Nothing Nothing)


listContents :: (Int, Int) -- ^ number of columns, column width in characters
             -> Bool -- ^ True if files beginning with "." should be included
             -> FilePath -- ^ directory to list
             -> IO ()
listContents (nCols, colWidth) flag dname = do
  showColours <- checkColourSupport
  cnts <- ls dname
  case cnts of
    Left eval   -> putStrLn ("Unable to access directory <" ++ dname ++ ">:\n" ++ show eval) >> exitFailure
    Right cnts' -> do
              let dcnts = lsToDir flag cnts'
                  dnames = mkColumn colWidth (Just '/') (dirs dcnts)
                  xnames = mkColumn colWidth (Just '*') (executables dcnts)
                  lnames = mkColumn colWidth (Just '@') (links dcnts)
                  fnames = mkColumn colWidth Nothing    (files dcnts)

                  wrapColor col action = do
                    when showColours $ setSGR [SetColor Foreground Dull col]
                    _ <- action
                    when showColours $ setSGR []

              unless (null dnames) $ wrapColor Cyan $ mapM_ putStrLn (columnToLine nCols dnames)
              unless (null xnames) $ wrapColor Red $ mapM_ putStrLn (columnToLine nCols xnames)
              unless (null lnames) $ wrapColor Magenta $ mapM_ putStrLn (columnToLine nCols lnames)
              unless (null fnames) $ mapM_ putStrLn (columnToLine nCols fnames)
              exitSuccess

-- Do we want to turn off colour support?
-- Based on https://no-color.org/
--
checkColourSupport :: IO Bool
checkColourSupport = do

  check <- hSupportsANSIColor stdout
  if check then
    do
      val <- fromMaybe "" <$> lookupEnv "NO_COLOR"
      case val of
        "" -> pure True
        _ -> pure False
  else
    pure False

usage :: IO ()
usage = getProgName >>= \n -> 
        putStrLn ("Usage: " ++ n ++ " [-h|-a|-v] [directory]") >>
        exitFailure

reportVersion :: IO ()
reportVersion = do
  name <- getProgName
  putStrLn (name <> ": v" <> showVersion P.version <> " (" <>
           SI.compilerName <> " " <> showVersion SI.fullCompilerVersion <>
           " " <> SI.os <> " " <> SI.arch <> ")")
  exitSuccess


{-
Simple heuristics for getting sensible-looking columns.

We try a minimum column width of 15 characters
and aim for 4 columns. The calculation isn't doing
quite what I expect.
-}

possibleWidths :: [Double]
possibleWidths = [15 .. 35]

getColumnSizing :: Int -> (Int, Int)
getColumnSizing tWidth = (nC, floor frac)
    where
      cWidths = reverse possibleWidths
      nCols = map (fromIntegral tWidth /) cWidths
      sels = dropWhile (<4.0) nCols
      c0 = case nCols of
        c: _ -> floor c
        _ -> error "possibleWidths is empty!"

      nC = case sels of
             s0:_ -> floor s0
             _ -> c0

      frac = fromIntegral (tWidth - nC + 1) / fromIntegral nC :: Double


data Args = Args {
     help :: Bool,
     version :: Bool,
     allFiles :: Bool,
     location :: String }

defArgs :: Args
defArgs = Args { help = False, version = False, allFiles = True, location = "." }


-- Muddle through without a proper parser for now
processArgs :: [String] -> Maybe Args
processArgs [] = Just defArgs
processArgs ["-h"] = Just $ defArgs { help = True }
processArgs ["-a"] = Just $ defArgs { allFiles = False }
processArgs ["-v"] = Just $ defArgs { version = True }
processArgs [loc] = Just $ defArgs { location = loc }
processArgs ["-a", loc] = Just $ defArgs { allFiles = False, location = loc }
processArgs ["-v", loc] = Just $ defArgs { version = True, location = loc }  -- could make this an error
processArgs _ = Nothing

main :: IO ()
main = do
  mArgs <- processArgs <$> getArgs
  case mArgs of
    Just args -> do
      if help args
      then usage
      else if version args
           then reportVersion
           else do
             cInfo <- fmap getColumnSizing terminalWidth
             listContents cInfo (allFiles args) (location args)

    Nothing -> usage
