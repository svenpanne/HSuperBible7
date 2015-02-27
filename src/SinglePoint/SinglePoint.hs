-- SinglePoint.hs, see listings 2.3 - 2.7 in the OpenGL SuperBible, 6th ed.
-- Adapted from singlepoint.cpp which is (c) 2012-2013 Graham Sellers.

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
init = return $ appInfo { title = "OpenGL SuperBible - Single Point" }

startup :: State -> IO ()
startup state = do
  let vs_source = unlines
        [ "#version 430 core                             "
        , "                                              "
        , "void main(void)                               "
        , "{                                             "
        , "    gl_Position = vec4(0.0, 0.0, 0.0, 1.0);   "
        , "}                                             " ]
      fs_source = unlines
        [ "#version 430 core                             "
        , "                                              "
        , "out vec4 color;                               "
        , "                                              "
        , "void main(void)                               "
        , "{                                             "
        , "    color = vec4(0.0, 0.8, 1.0, 1.0);         "
        , "}                                             " ]

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

  pointSize $= 40
  drawArrays Points 0 1

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
