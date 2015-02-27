-- TessellatedTri.hs, see listing 3.9 in the OpenGL SuperBible, 6th ed.
-- Adapted from tessellatedtri.cpp which is (c) 2012-2013 Graham Sellers.

module Main ( main ) where

import Data.IORef ( IORef, newIORef )
import Foreign.Marshal.Array ( withArray )
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw.Core43 ( glClearBufferfv, gl_COLOR )
import SB6

data State = State
  { programRef :: IORef Program
  , vaoRef :: IORef VertexArrayObject }

init :: IO AppInfo
init = return $ appInfo { title = "OpenGL SuperBible - Tessellation and Geometry Shaders" }

startup :: State -> IO ()
startup state = do
  let vs_source = unlines
        [ "#version 430 core                                                 "
        , "                                                                  "
        , "void main(void)                                                   "
        , "{                                                                 "
        , "    const vec4 vertices[] = vec4[](vec4( 0.25, -0.25, 0.5, 1.0),  "
        , "                                   vec4(-0.25, -0.25, 0.5, 1.0),  "
        , "                                   vec4( 0.25,  0.25, 0.5, 1.0)); "
        , "                                                                  "
        , "    gl_Position = vertices[gl_VertexID];                          "
        , "}                                                                 " ]
      tcs_source = unlines
        [ "#version 430 core                                                                 "
        , "                                                                                  "
        , "layout (vertices = 3) out;                                                        "
        , "                                                                                  "
        , "void main(void)                                                                   "
        , "{                                                                                 "
        , "    if (gl_InvocationID == 0)                                                     "
        , "    {                                                                             "
        , "        gl_TessLevelInner[0] = 5.0;                                               "
        , "        gl_TessLevelOuter[0] = 5.0;                                               "
        , "        gl_TessLevelOuter[1] = 5.0;                                               "
        , "        gl_TessLevelOuter[2] = 5.0;                                               "
        , "    }                                                                             "
        , "    gl_out[gl_InvocationID].gl_Position = gl_in[gl_InvocationID].gl_Position;     "
        , "}                                                                                 " ]
      tes_source = unlines
        [ "#version 430 core                                                                 "
        , "                                                                                  "
        , "layout (triangles, equal_spacing, cw) in;                                         "
        , "                                                                                  "
        , "void main(void)                                                                   "
        , "{                                                                                 "
        , "    gl_Position = (gl_TessCoord.x * gl_in[0].gl_Position) +                       "
        , "                  (gl_TessCoord.y * gl_in[1].gl_Position) +                       "
        , "                  (gl_TessCoord.z * gl_in[2].gl_Position);                        "
        , "}                                                                                 " ]
      gs_source = unlines
        [ "#version 430 core                                                                 "
        , "                                                                                  "
        , "layout (triangles) in;                                                            "
        , "layout (points, max_vertices = 3) out;                                            "
        , "                                                                                  "
        , "void main(void)                                                                   "
        , "{                                                                                 "
        , "    int i;                                                                        "
        , "                                                                                  "
        , "    for (i = 0; i < gl_in.length(); i++)                                          "
        , "    {                                                                             "
        , "        gl_Position = gl_in[i].gl_Position;                                       "
        , "        EmitVertex();                                                             "
        , "    }                                                                             "
        , "}                                                                                 " ]
      fs_source = unlines
        [ "#version 430 core                                                 "
        , "                                                                  "
        , "out vec4 color;                                                   "
        , "                                                                  "
        , "void main(void)                                                   "
        , "{                                                                 "
        , "    color = vec4(0.0, 0.8, 1.0, 1.0);                             "
        , "}                                                                 " ]

  program <- createProgram
  programRef state $= program

  vs <- createShader VertexShader
  shaderSourceBS vs $= packUtf8 vs_source
  compileShader vs

  tcs <- createShader TessControlShader
  shaderSourceBS tcs $= packUtf8 tcs_source
  compileShader tcs

  tes <- createShader TessEvaluationShader
  shaderSourceBS tes $= packUtf8 tes_source
  compileShader tes

  gs <- createShader GeometryShader
  shaderSourceBS gs $= packUtf8 gs_source
  compileShader gs

  fs <- createShader FragmentShader
  shaderSourceBS fs $= packUtf8 fs_source
  compileShader fs

  mapM_ (attachShader program) [vs, tcs, tes, gs, fs]
  linkProgram program
  deleteObjectNames [vs, tcs, tes, gs, fs]

  vao <- genObjectName
  vaoRef state $= vao
  bindVertexArrayObject $= Just vao

render :: State -> Double -> IO ()
render state _currentTime = do
  withArray [ 0, 0.25, 0, 1 ] $
    glClearBufferfv gl_COLOR 0

  p <- get (programRef state)
  currentProgram $= Just p

  pointSize $= 5
  drawArrays Patches 0 3

shutdown :: State -> IO ()
shutdown state = do
  deleteObjectName =<< get (vaoRef state)
  deleteObjectName =<< get (programRef state)

main :: IO ()
main = do
  program <- newIORef undefined
  vao <- newIORef undefined
  let state = State { programRef = program, vaoRef = vao }
  run $ app
    { SB6.init = Main.init
    , SB6.startup = Main.startup state
    , SB6.render = Main.render state
    , SB6.shutdown = Main.shutdown state
    }
