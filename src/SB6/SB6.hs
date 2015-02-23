module SB6 (
  Application(..), app,
  APPINFO(..), appInfo,
  run
) where

import Graphics.Rendering.OpenGL.GL
import Graphics.UI.GLUT as GLUT

data Application = Application {
  init :: IO APPINFO,
  render :: Double -> IO ()
  }

app :: Application
app = Application {
  SB6.init = defaultInit,
  render = defaultRender
  }

defaultInit :: IO APPINFO
defaultInit = return appInfo

defaultRender :: Double -> IO ()
defaultRender _currentTime = return ()

data APPINFO = APPINFO {
  title :: String,
  windowSize :: Size,
  version :: (Int, Int),
  samples :: Int,
  fullscreen :: Bool,
  vsync :: Bool,
  cursor :: Bool,
  stereo :: Bool,
  debug :: Bool
  }

appInfo :: APPINFO
appInfo = APPINFO {
  title = "SuperBible6 Example",
  SB6.windowSize  = Size 800 600,
  -- TODO: For __APPLE__ => (3, 2)
  version = (4, 3),
  SB6.samples = 0,
  fullscreen  = False,
  vsync  = False,
  SB6.cursor  = True,
  SB6.stereo  = False,
  -- TODO: For _DEBUG => True
  debug  = False
  }

run :: Application -> IO ()
run theApp = do
  (progName, _args) <- getArgsAndInitialize
  theAppInfo <- SB6.init theApp
  initialDisplayMode $= [ DoubleBuffered ]
  initialWindowSize $= SB6.windowSize theAppInfo
  initialContextVersion $= version theAppInfo
  initialContextProfile $= [ CoreProfile ]
  initialContextFlags $= [ ForwardCompatibleContext ] ++
    (if debug theAppInfo then [ DebugContext ] else [] )
  _ <- createWindow (title theAppInfo)
  displayCallback $= display (render theApp)
  mainLoop

display :: (Double -> IO ()) -> IO ()
display theRender = do
  t <- get elapsedTime
  theRender (fromIntegral t / 1000)
  swapBuffers
  postRedisplay Nothing
