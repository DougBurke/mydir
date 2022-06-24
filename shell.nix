{ compiler ? "ghc8107"
}:
(import ./release.nix { compiler = compiler; }).shell
