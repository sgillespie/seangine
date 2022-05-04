{
  ghc,
  nixPkgsVersion ? "249c76eb6787420b2178a3ba4cc64c4d9c4a5997", # pin
  nixPkgsSha256 ? "sha256:1lq2py428bfs6vv7qikzgh7hqyxw17c65dnfi35m46kjnidrrcbh",
}:

let
  tarball = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${nixPkgsVersion}.tar.gz";
    sha256 = nixPkgsSha256;
  };
  nixpkgs = import tarball {};
  inherit (nixpkgs) haskell;
in

haskell.lib.buildStackProject {
  inherit ghc;
  name = "myEnv";
  buildInputs = with nixpkgs; [
    SDL2
    glslang
    spirv-headers
    spirv-tools
    vulkan-extension-layer
    vulkan-headers
    vulkan-loader
    vulkan-tools
    vulkan-tools-lunarg
    vulkan-validation-layers
    zlib
  ];
}
