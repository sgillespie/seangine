{-# LANGUAGE QuasiQuotes #-}
module Graphics.Seangine.GraphicsPipeline.VertexShader
  (vertexShaderCode) where

import Vulkan.Utils.ShaderQQ.GLSL.Glslang (vert)
import Prelude
import Data.ByteString (ByteString())

vertexShaderCode :: ByteString
vertexShaderCode
  = [vert|
      #version 460
      #extension GL_EXT_debug_printf : enable

      struct ObjectData {
        mat4 transform;
      };

      layout(set = 0, binding = 0) uniform UniformBufferObject {
        mat4 model;
        mat4 view;
        mat4 projection;
      } uniformObject;

      layout(std140,set = 1, binding = 0) readonly buffer ObjectBuffer{
        ObjectData[] objects;
      } objectBuffer;

      layout(location = 0) in vec3 inPosition;
      layout(location = 1) in vec3 inNormal;
      layout(location = 2) in vec3 inColor;

      layout(location = 0) out vec3 fragColor;

      void main() {
        mat4 transform = objectBuffer.objects[gl_BaseInstance].transform;

        gl_Position =
          uniformObject.projection *
          uniformObject.view *
          uniformObject.model *
          transform *
          vec4(inPosition, 1.0);

        fragColor = inColor;
      }
    |]
