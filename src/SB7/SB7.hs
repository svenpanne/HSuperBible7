module SB7 (
  module Graphics.Rendering.OpenGL,
  module SB7.Application
) where

import Graphics.Rendering.OpenGL
-- The hiding is needed to avoid an incorrect warning with GHC 7.0.4.
import SB7.Application hiding (extensionSupported)
