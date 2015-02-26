-- SimpleClear.hs, see listings 2.1 - 2.2 in the OpenGL SuperBible, 6th ed.
-- Adapted from simpleclear.cpp which is (c) 2012-2013 Graham Sellers.

module Main ( main ) where

import Foreign.Marshal.Array ( withArray )
import Graphics.Rendering.OpenGL.Raw.Core43 ( glClearBufferfv, gl_COLOR )
import SB6

init :: IO AppInfo
init = return $ appInfo { title = "OpenGL SuperBible - Simple Clear" }

render :: Double -> IO ()
render currentTime = do
  withArray [ realToFrac (sin currentTime) * 0.5 + 0.5
            , realToFrac (cos currentTime) * 0.5 + 0.5
            , 0
            , 1 ] $
    glClearBufferfv gl_COLOR 0

main :: IO ()
main = run $ app
  { SB6.init = Main.init
  , SB6.render = Main.render
  }
