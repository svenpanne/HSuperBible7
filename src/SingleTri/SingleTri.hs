-- SingleTri.hs, see listings 2.8 - 2.9 in the OpenGL SuperBible, 7th ed.
-- Adapted from singletri.cpp which is (c) 2012-2015 Graham Sellers.

module Main ( main ) where

import Foreign.Marshal.Array ( withArray )
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw.Core42 ( glClearBufferfv, gl_COLOR )
import SB7

data State = State
  { program :: Program
  , vao :: VertexArrayObject
  }

init :: IO AppInfo
init = return $ appInfo { title = "OpenGL SuperBible - Single Triangle" }

startup :: IO State
startup = do
  let vs_source = unlines
        [ "#version 420 core                                                 "
        , "                                                                  "
        , "void main(void)                                                   "
        , "{                                                                 "
        , "    const vec4 vertices[] = vec4[](vec4( 0.25, -0.25, 0.5, 1.0),  "
        , "                                   vec4(-0.25, -0.25, 0.5, 1.0),  "
        , "                                   vec4( 0.25,  0.25, 0.5, 1.0)); "
        , "                                                                  "
        , "    gl_Position = vertices[gl_VertexID];                          "
        , "}                                                                 " ]
      fs_source = unlines
        [ "#version 420 core                                                 "
        , "                                                                  "
        , "out vec4 color;                                                   "
        , "                                                                  "
        , "void main(void)                                                   "
        , "{                                                                 "
        , "    color = vec4(0.0, 0.8, 1.0, 1.0);                             "
        , "}                                                                 " ]

  theProgram <- createProgram
  fs <- createShader FragmentShader
  shaderSourceBS fs $= packUtf8 fs_source
  compileShader fs

  vs <- createShader VertexShader
  shaderSourceBS vs $= packUtf8 vs_source
  compileShader vs

  mapM_ (attachShader theProgram) [ vs, fs ]

  linkProgram theProgram

  theVao <- genObjectName
  bindVertexArrayObject $= Just theVao

  return $ State { program = theProgram, vao = theVao }

render :: State -> Double -> IO ()
render state _currentTime = do
  withArray [ 0, 0.25, 0, 1 ] $
    glClearBufferfv gl_COLOR 0

  currentProgram $= Just (program state)
  drawArrays Triangles 0 3

shutdown :: State -> IO ()
shutdown state = do
  deleteObjectName $ vao state
  deleteObjectName $ program state

main :: IO ()
main = run $ app
  { SB7.init = Main.init
  , SB7.startup = Main.startup
  , SB7.render = Main.render
  , SB7.shutdown = Main.shutdown
  }
