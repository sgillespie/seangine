{pkgs, ...}: {
  name = "seangine";
  compiler-nix-name = "ghc928";

  crossPlatforms = p: pkgs.lib.optionals pkgs.stdenv.hostPlatform.isx86_64 ([
    p.mingwW64
  ]);

  shell.tools = {
    cabal = "latest";
    fourmolu = "latest";
    hlint = "latest";
    haskell-language-server = "latest";
  };
}
