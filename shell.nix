{ compiler ? "ghc902"
}:
(import ./release.nix { compiler = compiler; }).shell
