{ pkgs,
  ...
}: {
  name = "seangine";
  compiler-nix-name = "ghc927";

  crossPlatforms = p: pkgs.lib.optionals pkgs.stdenv.hostPlatform.isx86_64 ([
    # TODO: This does not seem to work with Vulkan
    # p.mingwW64
  ] ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [
    # p.musl64
  ]);
}
