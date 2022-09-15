{-# LANGUAGE QuasiQuotes #-}
module Graphics.Seangine.GraphicsPipeline.VertexShader
  (vertexShaderCode) where

import Vulkan.Utils.ShaderQQ.GLSL.Glslang (vert)
import Prelude
import Data.ByteString (ByteString())

vertexShaderCode :: ByteString
vertexShaderCode
  = [vert|
      #version 450
      #extension GL_ARB_separate_shader_objects: enable

      layout(push_constant) uniform constants {
        mat4 transform;
      } pushConstants;

      layout(binding = 0) uniform UniformBufferObject {
        mat4 model;
        mat4 view;
        mat4 projection;
      } uniformObject;

      layout(location = 0) in vec3 inPosition;
      layout(location = 1) in vec3 inNormal;
      layout(location = 2) in vec3 inColor;

      layout(location = 0) out vec3 fragColor;

      void main() {
        gl_Position =
          uniformObject.projection *
          uniformObject.view *
          uniformObject.model *
          pushConstants.transform *
          vec4(inPosition, 1.0);

        fragColor = inColor;
      }
    |]
