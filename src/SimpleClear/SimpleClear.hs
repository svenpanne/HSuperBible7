-- SimpleClear.hs, see listing 2.1 in the OpenGL SuperBible, 7th ed.
-- Adapted from simpleclear.cpp which is (c) 2012-2015 Graham Sellers.

module Main ( main ) where

import SB7

init :: IO AppInfo
init = return $ appInfo { title = "OpenGL SuperBible - Simple Clear" }

render :: s -> Double -> IO ()
render _state _currentTime =
  clearBuffer $ ClearColorBufferFloat 0 (Color4 1 0 0 1)

main :: IO ()
main = run $ app
  { SB7.init = Main.init
  , SB7.render = Main.render
  }
