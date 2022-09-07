{
  pkgs,
  index-state ? "2022-09-05T00:00:00Z"
}:

(final: prev: {
  seangineProject =
    final.haskell-nix.project' {
      inherit index-state;
      
      src = ./.;
      compiler-nix-name = "ghc902";

      modules = [{
        packages.seangine.components.library.build-tools = pkgs.lib.mkForce [pkgs.glslang];
      }];
              
      shell.tools = {
        cabal = { inherit index-state; };
        hlint = { inherit index-state; };
        haskell-language-server = { inherit index-state; };
      };
      
      shell.buildInputs = with pkgs; [
        watchman
        nixpkgs-fmt
      ];
    };
})
