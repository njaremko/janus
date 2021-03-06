{
  description = "A very basic flake";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            # This overlay adds our project to pkgs
            janus =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc8107";
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; };
        flake = pkgs.janus.flake { };
        index-state = "2022-07-21T00:00:00Z";
      in
      flake // {
        # Built by `nix build .`
        defaultPackage = flake.packages."janus:exe:janus";

        # This is used by `nix develop .` to open a shell for use with
        # `cabal`, `hlint` and `haskell-language-server`
        devShell = pkgs.janus.shellFor {
          tools = {
            cabal = { inherit index-state; version = "latest"; materialized = ./.nix/cabal.materialized; };
            hlint = { inherit index-state; version = "latest"; materialized = ./.nix/hlint.materialized; };
            haskell-language-server = { inherit index-state; version = "latest"; materialized = ./.nix/haskell-language-server.materialized; };
            hoogle = { inherit index-state; version = "latest"; materialized = ./.nix/hoogle.materialized; };
          };
        };
      });
}
