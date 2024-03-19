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

        build-mentat-wasm-haskell = pkgs.runCommand "make-hs-wasm" {
          buildInputs = [
            wasm.packages.${system}.all_9_8
            pkgs.cabal-install
          ];
        } ''
          cabal --config-file=conf/cabal/config build --project-file=cabal-wasm.project mentat-wasm
          '';

        build-mentat-wasm = pkgs.runCommand "make-wasm-interface" {
          buildInputs = [
            pkgs.wizer
            build-mentat-wasm-haskell
          ];
        } ''
          wizer --allow-wasi --wasm-bulk-memory true "$(cabal --config-file=conf/cabal/config --project-file=cabal-wasm.project list-bin -v0 mentat-wasm)" -o mentat-interop.wasmist-bin -v0 mentat-wasm)" -o src/frontend/wasm/mentat-interop.wasm
          '';

        build-faraday = pkgs.runCommand "make-faraday" {
          buildInputs = [
            pkgs.nodejs
            pkgs.nodePackages.pnpm
            pkgs.nodePackages.typescript
            build-mentat-wasm
          ];

        } ''
          mkdir -p build
          npm run build
          '';

        # DON'T FORGET TO PUT YOUR PACKAGE NAME HERE, REMOVING `throw`
        packageName = "Faraday";

      in {
        packages.mentat-wasm-haskell = build-mentat-wasm-haskell;
        packages.mentat-wasm = build-mentat-wasm;
        defaultPackage = build-faraday;

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

