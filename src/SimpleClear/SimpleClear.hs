-- SimpleClear.hs, see listings 2.1 and 2.2 in the OpenGL SuperBible, 6th ed.
-- Adapted from simpleclear.cpp which is (c) Graham Sellers.

module Main ( main ) where

import Foreign.Marshal.Array
import Graphics.Rendering.OpenGL.Raw.Core43
import SB6

init :: IO AppInfo
init = return $ appInfo { title = "OpenGL SuperBible - Simple Clear" }

render :: Double -> IO ()
render currentTime = do
  -- This is actually a combination of listings 2.1 and 2.2.
  let red   = [ 1, 0, 0, 1 ]
      color = [ realToFrac (sin currentTime) * 0.5 + 0.5
              , realToFrac (cos currentTime) * 0.5 + 0.5
              , 0
              , 1 ]
  withArray (if True then color else red) $
    glClearBufferfv gl_COLOR 0

main :: IO ()
main = run $ app
  { SB6.init = Main.init
  , SB6.render = Main.render
  }
