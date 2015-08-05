-- SimpleClear.hs, see listing 2.1 in the OpenGL SuperBible, 7th ed.
-- Adapted from simpleclear.cpp which is (c) 2012-2015 Graham Sellers.

module Main ( main ) where

import Foreign.Marshal.Array ( withArray )
import Graphics.Rendering.OpenGL.Raw.Core43 ( glClearBufferfv, gl_COLOR )
import SB7

init :: IO AppInfo
init = return $ appInfo { title = "OpenGL SuperBible - Simple Clear" }

render :: s -> Double -> IO ()
render _state _currentTime =
  withArray [ 1, 0, 0, 1 ] $
    glClearBufferfv gl_COLOR 0

main :: IO ()
main = run $ app
  { SB7.init = Main.init
  , SB7.render = Main.render
  }
