{pkgs, ...}: {
  name = "seangine";
  compiler-nix-name = "ghc928";

  crossPlatforms = p: pkgs.lib.optionals pkgs.stdenv.hostPlatform.isx86_64 ([
    # p.mingwW64
    # p.ghcjs # TODO GHCJS support for GHC 9.2
  ] ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [
    # p.musl64
  ]);

  shell.tools.cabal = "latest";
  shell.tools.hlint = "latest";
  shell.tools.haskell-language-server = "latest";
}
