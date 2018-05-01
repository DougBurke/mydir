{ mkDerivation, base, directory, filepath, process, stdenv, unix }:
mkDerivation {
  pname = "mydir";
  version = "1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base directory filepath process unix
  ];
  description = "A simple directory listing utility";
  license = stdenv.lib.licenses.publicDomain;
}
