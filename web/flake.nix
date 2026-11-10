{
  # Side track W (the emulators in the browser): GHC's WebAssembly backend
  # (ghc-wasm-meta, GHC 9.10 as the main dev shell's) and Node for the
  # headless tests. Its own flake, so the main flake.lock stays as it is.
  description = "rota in the browser: the toolchain";

  inputs.ghc-wasm-meta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";

  outputs = { self, ghc-wasm-meta }:
    let
      system = "x86_64-linux";
      pkgs = ghc-wasm-meta.inputs.nixpkgs.legacyPackages.${system};
    in {
      devShells.${system}.default = pkgs.mkShell {
        packages = [ ghc-wasm-meta.packages.${system}.all_9_10 pkgs.nodejs ];
      };
    };
}
