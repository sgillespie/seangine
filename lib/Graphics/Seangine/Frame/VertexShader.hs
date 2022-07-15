{-# LANGUAGE QuasiQuotes #-}
module Graphics.Seangine.Frame.VertexShader
  ( vertexShaderCode,
    debugVertexShaderCode
  ) where

import Vulkan.Utils.ShaderQQ.GLSL.Glslang (vert)

import Data.ByteString (ByteString())

vertexShaderCode :: ByteString
vertexShaderCode
  = [vert|
      #version 450
      #extension GL_ARB_separate_shader_objects: enable

      layout(binding = 0) uniform UniformBufferObject {
        mat4 model;
        mat4 view;
        mat4 projection;
      } uniformObject;

      layout(location = 0) in vec3 inPosition;
      layout(location = 1) in vec3 inColor;
      layout(location = 2) in vec2 inTexCoord;

      layout(location = 0) out vec3 fragColor;
      layout(location = 1) out vec2 fragTexCoord;

      void main() {
        gl_Position =
          uniformObject.projection *
          uniformObject.view *
          uniformObject.model *
          vec4(inPosition, 1.0);

        fragColor = inColor;
        fragTexCoord = inTexCoord;
      }
    |]

debugVertexShaderCode :: ByteString
debugVertexShaderCode
  = [vert|
      #version 450
      #extension GL_ARB_separate_shader_objects : enable
      #extension GL_EXT_debug_printf : enable

      layout(binding = 0) uniform UniformBufferObject {
        mat4 model;
        mat4 view;
        mat4 projection;
      } uniformObject;

      layout(location = 0) in vec2 inPosition;
      layout(location = 1) in vec3 inColor;

      layout(location = 0) out vec3 fragColor;

      void main() {
        debugPrintfEXT(
          "\nModel: \n%f\t%f\t%f\t%f\n%f\t%f\t%f\t%f\n%f\t%f\t%f\t%f\n%f\t%f\t%f\t%f\n\nView: \n%f\t%f\t%f\t%f\n%f\t%f\t%f\t%f\n%f\t%f\t%f\t%f\n%f\t%f\t%f\t%f\n\nProjection: \n%f\t%f\t%f\t%f\n%f\t%f\t%f\t%f\n%f\t%f\t%f\t%f\n%f\t%f\t%f\t%f",
          uniformObject.model[0][0],
          uniformObject.model[0][1],
          uniformObject.model[0][2],
          uniformObject.model[0][3],

          uniformObject.model[1][0],
          uniformObject.model[1][1],
          uniformObject.model[1][2],
          uniformObject.model[1][3],

          uniformObject.model[2][0],
          uniformObject.model[2][1],
          uniformObject.model[2][2],
          uniformObject.model[2][3],

          uniformObject.model[3][0],
          uniformObject.model[3][1],
          uniformObject.model[3][2],
          uniformObject.model[3][3],

          uniformObject.view[0][0],
          uniformObject.view[0][1],
          uniformObject.view[0][2],
          uniformObject.view[0][3],

          uniformObject.view[1][0],
          uniformObject.view[1][1],
          uniformObject.view[1][2],
          uniformObject.view[1][3],

          uniformObject.view[2][0],
          uniformObject.view[2][1],
          uniformObject.view[2][2],
          uniformObject.view[2][3],

          uniformObject.view[3][0],
          uniformObject.view[3][1],
          uniformObject.view[3][2],
          uniformObject.view[3][3],

          uniformObject.projection[0][0],
          uniformObject.projection[0][1],
          uniformObject.projection[0][2],
          uniformObject.projection[0][3],

          uniformObject.projection[1][0],
          uniformObject.projection[1][1],
          uniformObject.projection[1][2],
          uniformObject.projection[1][3],

          uniformObject.projection[2][0],
          uniformObject.projection[2][1],
          uniformObject.projection[2][2],
          uniformObject.projection[2][3],

          uniformObject.projection[3][0],
          uniformObject.projection[3][1],
          uniformObject.projection[3][2],
          uniformObject.projection[3][3]
        );
        gl_Position = uniformObject.model * vec4(inPosition, 0.0, 1.0);
        fragColor = inColor;
      }
    |]
