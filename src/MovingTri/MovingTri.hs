-- MovingTri.hs, see listings 3.1 - 3.2 in the OpenGL SuperBible, 6th ed.
-- Adapted from movingtri.cpp which is (c) 2012-2013 Graham Sellers.

module Main ( main ) where

import Data.IORef ( IORef, newIORef )
import Foreign.Marshal.Array ( withArray )
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw.Core43 (
  glClearBufferfv, gl_COLOR, glVertexAttrib4fv )
import SB6

data State = State
  { programRef :: IORef Program
  , vaoRef :: IORef VertexArrayObject }

init :: IO AppInfo
init = return $ appInfo { title = "OpenGL SuperBible - Moving Triangle" }

startup :: State -> IO ()
startup state = do
  let vs_source = unlines
        [ "#version 430 core                                                 "
        , "                                                                  "
        , "layout (location = 0) in vec4 offset;                             "
        , "                                                                  "
        , "void main(void)                                                   "
        , "{                                                                 "
        , "    const vec4 vertices[] = vec4[](vec4( 0.25, -0.25, 0.5, 1.0),  "
        , "                                   vec4(-0.25, -0.25, 0.5, 1.0),  "
        , "                                   vec4( 0.25,  0.25, 0.5, 1.0)); "
        , "                                                                  "
        , "    // Add 'offset' to our hard-coded vertex position             "
        , "    gl_Position = vertices[gl_VertexID] + offset;                 "
        , "}                                                                 " ]
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

  fs <- createShader FragmentShader
  shaderSourceBS fs $= packUtf8 fs_source
  compileShader fs

  mapM_ (attachShader program) [vs, fs]
  linkProgram program
  deleteObjectNames [vs, fs]

  vao <- genObjectName
  vaoRef state $= vao
  bindVertexArrayObject $= Just vao

render :: State -> Double -> IO ()
render state currentTime = do
  withArray [ realToFrac (sin currentTime) * 0.5 + 0.5
            , realToFrac (cos currentTime) * 0.5 + 0.5
            , 0
            , 1 ] $
    glClearBufferfv gl_COLOR 0

  p <- get (programRef state)
  currentProgram $= Just p

  withArray [ realToFrac (sin currentTime) * 0.5
            , realToFrac (cos currentTime) * 0.6
            , 0
            , 0 ] $
    glVertexAttrib4fv 0
  drawArrays Triangles 0 3

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
