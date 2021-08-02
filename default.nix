{ nixpkgs ? import <nixpkgs> {},
  compiler ? "ghc8104"
}:

let
  inherit (nixpkgs.haskell.packages.${compiler})
    base
    callPackage
    lib
    mkDerivation;
in

with nixpkgs;
with xorg;

mkDerivation {
  pname = "seangine";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  
  executableSystemDepends = [
    glfw3
    glm
    libX11
    libXxf86vm
    libXrandr
    libXi
    safe
    shaderc
    spirv-tools
    vulkan-headers
    vulkan-loader
    vulkan-tools
    vulkan-tools-lunarg
    vulkan-validation-layers
  ];
  executableHaskellDepends = with nixpkgs.haskell.packages.${compiler}; [
    base
    bytestring
    derive-storable-plugin
    derive-storable
    GLFW-b
    extra
    JuicyPixels
    linear
    managed
    resourcet
    transformers
    unliftio-core
    vector
    vulkan
    vulkan-utils
    VulkanMemoryAllocator
  ];
  
  description = "Vulkan renderer by and for Sean Gillespie";
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}

