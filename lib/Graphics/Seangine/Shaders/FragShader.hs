{-# LANGUAGE QuasiQuotes #-}
module Graphics.Seangine.Shaders.FragShader where

import Vulkan.Utils.ShaderQQ.GLSL.Glslang (frag)

import Data.ByteString (ByteString())

fragShaderCode :: ByteString
fragShaderCode
  = [frag|
      #version 450

      layout(location = 0) in vec3 fragColor;
      
      layout(location = 0) out vec4 outColor;

      void main() {
        outColor = vec4(fragColor, 1.0);
      }
    |]