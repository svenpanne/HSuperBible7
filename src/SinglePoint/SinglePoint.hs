module Main ( main ) where

import Data.IORef ( IORef, newIORef )
import Foreign.C.String ( withCString )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Array ( withArray )
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( nullPtr )
import Foreign.Storable ( peek )
import Graphics.Rendering.OpenGL ( get, ($=) )
import Graphics.Rendering.OpenGL.Raw.Core43
import SB6

data State = State
  { programRef :: IORef GLuint
  , vaoRef :: IORef GLuint }

init :: IO AppInfo
init = return $ appInfo { title = "OpenGL SuperBible - Single Point" }

startup :: State -> IO ()
startup state = do
  let vs_source = unlines
        [ "#version 420 core                             "
        , "                                              "
        , "void main(void)                               "
        , "{                                             "
        , "    gl_Position = vec4(0.0, 0.0, 0.0, 1.0);   "
        , "}                                             " ]
      fs_source = unlines
        [ "#version 420 core                             "
        , "                                              "
        , "out vec4 color;                               "
        , "                                              "
        , "void main(void)                               "
        , "{                                             "
        , "    color = vec4(0.0, 0.8, 1.0, 1.0);         "
        , "}                                             " ]

  program <- glCreateProgram
  programRef state $= program

  fs <- glCreateShader gl_FRAGMENT_SHADER
  withCString fs_source $ \buf ->
    with buf $ \bufBuf ->
      glShaderSource fs 1 bufBuf nullPtr
  glCompileShader fs

  vs <- glCreateShader gl_VERTEX_SHADER
  withCString vs_source $ \buf ->
    with buf $ \bufBuf ->
      glShaderSource vs 1 bufBuf nullPtr
  glCompileShader vs

  glAttachShader program vs
  glAttachShader program fs

  glLinkProgram program

  vao <- alloca $ \buf -> do
    glGenVertexArrays 1 buf
    peek buf
  vaoRef state $= vao
  glBindVertexArray vao

render :: State -> Double -> IO ()
render state _currentTime = do
  withArray [ 1, 0, 0, 1 ] $
    glClearBufferfv gl_COLOR 0
  glUseProgram =<< get (programRef state)
  glPointSize 40
  glDrawArrays gl_POINTS 0 1

shutdown :: State -> IO ()
shutdown state = do
  vao <- get (vaoRef state)
  with vao $ glDeleteVertexArrays 1
  glDeleteProgram =<< get (programRef state)

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
