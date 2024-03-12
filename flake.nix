{
  description = "A FOSS online graphing calculator inspired by Desmos";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    ghc-wasm-meta.url = "https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/archive/master/ghc-wasm-meta-master.tar.gz";
  };

  outputs = { self, nixpkgs, flake-utils, ghc-wasm-meta, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        lib = nixpkgs.lib;
        pkgs = nixpkgs.legacyPackages.${system};
        wasm = ghc-wasm-meta;



        # DON'T FORGET TO PUT YOUR PACKAGE NAME HERE, REMOVING `throw`
        packageName = "Faraday";

      in {

        

        devShells.default = pkgs.mkShell {
          buildInputs = [ 
            pkgs.nodejs
            # You can set the major version of Node.js to a specific one instead
            # of the default version
            # pkgs.nodejs-19_x

            # You can choose pnpm, yarn, or none (npm).
            pkgs.nodePackages.pnpm
            # pkgs.yarn

            pkgs.nodePackages.typescript
            pkgs.nodePackages.typescript-language-server
            pkgs.python311
            pkgs.cabal-install
            pkgs.ghc
            pkgs.wizer
            pkgs.haskellPackages.haskell-language-server
            pkgs.haskellPackages.hindent
          ] ++ (if system != "aarch64-darwin" then [
            wasm.packages.${system}.all_9_8
          ] else []);
        };
      });
}

