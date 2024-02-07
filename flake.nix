{
  description = "Building threepenny-gui with haskell.nix";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    # for caching you want to follow haskell.nix's nixpkgs-unstable pins.
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ haskellNix.overlay ];
          inherit (haskellNix) config;
        };

        # If we are building a haskell project (e.g. the current directory)
        # we'd use this to get the haskell packages from the current project.
        project = pkgs.haskell-nix.project' {
          compiler-nix-name = "ghc964";
          src = ./.;
          projectFileName = "cabal.project";

          shell.tools = {
            cabal = {};
          };
          shell.buildInputs =[
            pkgs.pkg-config
          ];
        };

        exes = project.hsPkgs.threepenny-gui.components.exes;

      in {
        packages = {
          inherit (exes) threepenny-examples-buttons;
          default = exes.threepenny-examples-buttons;
        };
        devShells = {
          default = project.shell;
        };
      }
    );

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = "true";
  };
}