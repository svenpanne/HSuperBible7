-- SimpleClear.hs (adapted from simpleclear.cpp which is (c) Graham Sellers)

module Main ( main ) where

import Foreign.Marshal.Utils
import Foreign.Ptr
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.Raw.Core43
import SB6

init :: IO APPINFO
init = return $ appInfo {
  title = "OpenGL SuperBible - Simple Clear"
  }

render :: Double -> IO ()
render _currentTime =
  with (Color4 1 0 0 1 :: Color4 GLfloat) $ \red ->
    glClearBufferfv gl_COLOR 0 (castPtr red)

main :: IO ()
main = run $ Application {
  SB6.init = Main.init,
  SB6.render = Main.render
  }
