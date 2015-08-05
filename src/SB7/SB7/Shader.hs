module SB7.Shader (
  load,
  linkFromShaders
) where

import Control.Exception ( bracketOnError )
import Control.Monad ( unless )
import qualified Data.ByteString as B
import Graphics.Rendering.OpenGL

load :: FilePath -> ShaderType -> IO Shader
load fileName shType =
  createShader shType `bracketOnError` deleteObjectName $ \shader -> do
    src <- B.readFile fileName
    shaderSourceBS shader $= src
    checked compileShader compileStatus shaderInfoLog (fileName ++ ": ") shader

linkFromShaders :: [Shader] -> IO Program
linkFromShaders shaders =
  createProgram `bracketOnError` deleteObjectName $ \program -> do
    mapM_ (\s -> do attachShader program s; deleteObjectName s) shaders
    checked linkProgram linkStatus programInfoLog "" program

checked :: (t -> IO ())
        -> (t -> GettableStateVar Bool)
        -> (t -> GettableStateVar String)
        -> String
        -> t
        -> IO t
checked action getStatus getInfoLog messagePrefix object = do
  action object
  ok <- get (getStatus object)
  unless ok $ do
    infoLog <- get (getInfoLog object)
    fail $ messagePrefix ++ infoLog
  return object
