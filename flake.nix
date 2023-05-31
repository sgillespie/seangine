{
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.feedback.url = "github:NorfairKing/feedback";

  outputs = { self, nixpkgs, flake-utils, haskellNix, feedback }:
    let
      supportedSystems = [
        "x86_64-linux"
      ];
    in
      flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [
          (final: prev: {
            vulkan-extension-layer = prev.vulkan-extension-layer.overrideAttrs
              (finalAttrs: prevAttrs: {
                setupHook = final.writeText "setup-hook" ''
                  addToSearchPath XDG_DATA_DIRS @out@/share
                  export XDG_DATA_DIRS
                '';
              });
          })
          
          haskellNix.overlay
          (final: prev: {
            hixProject =
              final.haskell-nix.hix.project {
                src = ./.;
                evalSystem = "x86_64-linux";
                
                modules = [{
                  packages.seangine.components.library.build-tools =
                    pkgs.lib.mkForce (with pkgs; [
                      glslang
                    ]);
                }];

                shell = {
                  withHoogle = true;
                  
                  tools = {
                    cabal = { };
                    haskell-language-server = { };
                    hlint = { };
                  };

                  nativeBuildInputs = with pkgs; [
                    spirv-headers
                    spirv-tools
                    vulkan-extension-layer
                    vulkan-tools
                    vulkan-tools-lunarg
                    vulkan-validation-layers
                  ];

                  buildInputs = with pkgs; [
                    haskell-language-server
                    feedback.packages.${system}.default
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
