{
  ghc,
  nixPkgsVersion ? "nixos-21.11",
}:

let tarball = "https://github.com/NixOS/nixpkgs/archive/${nixPkgsVersion}.tar.gz";
in
with (import (fetchTarball tarball) {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "myEnv";
  buildInputs = [
    pkgconfig
    SDL2
    vulkan-headers
    vulkan-loader
    vulkan-tools
    vulkan-tools-lunarg
    vulkan-validation-layers
  ];
}
