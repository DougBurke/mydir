{ compiler ? "ghc96"
}:
(import ./release.nix { compiler = compiler; }).exe
