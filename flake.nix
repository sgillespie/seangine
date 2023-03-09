{
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.feedback.url = "github:NorfairKing/feedback";

  outputs = { self, nixpkgs, flake-utils, haskellNix, feedback }:
    let
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      index-state = "2022-12-27T00:00:00Z";
    in
      flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            hixProject =
              final.haskell-nix.hix.project {
                src = ./.;
                evalSystem = "x86_64-linux";
                inherit index-state;

                
                modules = [{
                  packages.seangine.components.library.build-tools =
                    pkgs.lib.mkForce [pkgs.glslang];
                }];

                shell = {
                  withHoogle = true;
                  
                  tools = {
                    cabal = { inherit index-state; };
                    hlint = { inherit index-state; };
                  };

                  buildInputs = with pkgs; [
                    feedback.packages.${system}.default
                    nixpkgs-fmt
                    renderdoc
                  ];
                    
                };
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.hixProject.flake {};
      in flake // {
        legacyPackages = pkgs;
        packages.default = flake.packages."seangine:exe:seangine";
      });

  nixConfig = {
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
  };
}
