{ sources ? import ./nix/sources.nix
# , nixpkgs ? import sources.nixpkgs {}
, pkgs ? import sources.nixpkgs {}
, compiler ? "ghc96"
}:

let
  # inherit (nixpkgs) pkgs;
  
  # since we are in a sub-directory
  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      "mydir" =
        hself.callCabal2nix "mydir" (gitignore ./.) {};

      # formatting =
      #   hself.callHackage "formatting" "7.1.2" {};
      # formatting = hself.formatting_7_1_2;
      
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."mydir"
    ];
    buildInputs = [
      pkgs.haskellPackages.cabal-install
      # pkgs.haskellPackages.haskell-language-server
      pkgs.haskellPackages.hlint
      pkgs.niv
    ];
    # withHoogle = true;
  };

  exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages."mydir");

in
  {
   inherit shell;
   inherit exe;
   inherit myHaskellPackages;
   "mydir" = myHaskellPackages."mydir";
  }

