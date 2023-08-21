{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  description = "An opinionated directory listing tool";
  inputs.nixpkgs.url = "nixpkgs";
  outputs = { self, nixpkgs }:
    let
      # supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });
    in
    {
      overlay = (final: prev: {
        mydir = final.haskellPackages.callCabal2nix "mydir" ./. {};
      });
      packages = forAllSystems (system: {
         mydir = nixpkgsFor.${system}.mydir;
      });
      defaultPackage = forAllSystems (system: self.packages.${system}.mydir);
      checks = self.packages;
      devShell = forAllSystems (system: let haskellPackages = nixpkgsFor.${system}.haskellPackages;
        in haskellPackages.shellFor {
          packages = p: [self.packages.${system}.mydir];
          # withHoogle = true;
          buildInputs = with haskellPackages; [
            # haskell-language-server
	    ghcid
            hlint
            cabal-install
          ];
        # Change the prompt to show that you are in a devShell
        shellHook = ''
  echo -e "*** \e[1;32mWelcome to mydir\e[0m ***"
  export PS1='mydir:\A \e[1;34m\w\e[0m '
	'';
        });
  };
}
