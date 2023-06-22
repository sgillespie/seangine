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

    in
      flake-utils.lib.eachSystem supportedSystems (system:
      let
        haskellProject = (final: prev: {
            hixProject =
              final.haskell-nix.hix.project {
                src = ./.;
                evalSystem = "x86_64-linux";
              };
          });

        feedbackOverlay = (final: prev: { feedback = feedback.packages.${system}.default; });

        checks = (final: prev: {
          fourmolu = final.haskell-nix.tool
            final.hixProject.args.compiler-nix-name
            "fourmolu"
            { version = "latest"; };

          hlintCheck = prev.runCommand "hlint-check" { buildInputs = [final.hlint]; } ''
            cd "${final.hixProject.args.src}"

            hlint app src test
            if [[ "$?" -eq 0 ]]; then
              touch $out
            fi
          '';

          fourmoluCheck =
            prev.runCommand
              "fourmolu-check"
              {
                buildInputs = [ final.fourmolu ];
              }
              ''
                cd "${final.hixProject.args.src}"

                fourmolu --mode check app src test
                if [[ "$?" -eq 0 ]]; then
                  touch $out
                fi
              '';
        });

        overlays = [
          haskellNix.overlay
          haskellProject
          checks
          feedbackOverlay
        ];

        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.hixProject.flake {};
      in flake // {
        checks = {
          inherit (pkgs) hlintCheck fourmoluCheck;
        };
        legacyPackages = pkgs;
        packages = flake.packages // {
          default = flake.packages."seangine:exe:seangine";
          inherit (pkgs) hlintCheck fourmoluCheck;
        };
      });

  nixConfig = {
    extra-substituters = [
      "https://sgillespie.cachix.org"
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "sgillespie.cachix.org-1:Zgif/WHW2IzHqbMb1z56cMmV5tLAA+zW9d5iB5w/VU4="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = "true";
  };
}
