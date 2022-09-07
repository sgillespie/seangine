{
  description = "A very basic flake";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          seangineProject =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc902";

              modules = [{
                packages.seangine.components.library.build-tools = pkgs.lib.mkForce [pkgs.glslang];
              }];
              
              shell.tools = {
                cabal = {};
                hlint = {};
                haskell-language-server = {};
              };

              shell.buildInputs = with pkgs; [
                nixpkgs-fmt
              ];
            };
        })
      ];

      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.seangineProject.flake { };
    in flake // {
      packages.default = flake.packages."seangine:exe:seangine";
    });
}
