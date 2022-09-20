# Seangine
> An experimental 3D rendering engine in Vulkan

This project was created to try to learn the Vulkan 3D graphics library in Haskell. Therefore, it is not complete or production quality.

## Introduction
Seangine is a highish level interface to the Vulkan API. The design goals are

 1. Learn Vulkan in Haskell
 2. Experiment with game development in Haskell
 3. Explore using popular 3rd-party physics libraries (ex. Bullet)
 4. Explore implementing alternate update-loop paradigms (ex. Functional reactive)

## Prerequisites
Seangine has only been tested in NixOS, but it should work in any environment. If running in a Nix environment, there are no other dependencies.
Non-nix users will need to be sure the following packages are installed

 * glfw3
 * shaderc (or glslc)
 * spirv-tools
 * vulkan-headers
 * vulkan-loader
 * vulkan-tools
 * vulkan-tools-lunarg
 * vulkan-validation-layers
 * renderdoc (optional but recommended)

## Getting Started
Enter a Nix shell

    nix-shell
    
Run the project
    
    cabal run
    
This will run Seangine and output basic stats:

![Peek 2021-07-27 22-36](https://user-images.githubusercontent.com/139144/127254717-8c5eb04c-6003-4ca9-9807-9e39e7a457e9.gif)


    Using device: "AMD RADV POLARIS10 (ACO)" (PHYSICAL_DEVICE_TYPE_DISCRETE_GPU)
    Processed: 52437 Frames
    Ran for: 6.520799395999347 Secs
    Average: 8041.4987205665675 FPS

## Building
This has only been tested in NixOS. In order to build, run

    nix-build
    
This will build the program and place the binary in `result/bin/seangine`

## Acknowlegdements
This project is largely based on:

 * The [examples](https://github.com/expipiplus1/vulkan/tree/master/examples) in [vulkan](https://github.com/expipiplus1/vulkan)
 * [Vulkan Tutorial](https://vulkan-tutorial.com/)

Furthermore, none of this would be possible without the excellent [Vulkan](https://www.vulkan.org/) API by the [Khronos Group](https://www.khronos.org/)
