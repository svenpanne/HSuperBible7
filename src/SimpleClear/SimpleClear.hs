-- SimpleClear.hs (adapted from simpleclear.cpp which is (c) Graham Sellers)

module Main ( main ) where

import Foreign.Marshal.Array
import Graphics.Rendering.OpenGL.Raw.Core43
import SB6

init :: IO APPINFO
init = return $ appInfo { title = "OpenGL SuperBible - Simple Clear" }

render :: Double -> IO ()
render _currentTime =
  withArray ([1, 0, 0, 1] :: [GLfloat]) $
    glClearBufferfv gl_COLOR 0

main :: IO ()
main = run $ app {
  SB6.init = Main.init,
  SB6.render = Main.render }
