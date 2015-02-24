{-# LANGUAGE CPP #-}
module SB6 (
  Application(..), app,
  APPINFO(..), appInfo,
  run
) where

import Control.Monad
import Data.Time.Clock
import System.IO
import Graphics.UI.GLUT as GLUT

data Application = Application {
  init :: IO APPINFO,
  startup :: IO (),
  render :: Double -> IO (),
  shutdown :: IO (),
  onResize :: Size -> IO (),
  onKey :: Either SpecialKey Char -> KeyState -> IO (),
  onMouseButton :: MouseButton -> KeyState -> IO (),
  onMouseMove :: Position -> IO () }

app :: Application
app = Application {
  SB6.init = return appInfo,
  startup = return (),
  render = \_currentTime -> return (),
  shutdown = return (),
  onResize = \_size -> return (),
  onKey = \_key _keyState -> return (),
  onMouseButton = \_mouseButton _keyState -> return (),
  onMouseMove = \_position -> return () }

data APPINFO = APPINFO {
  title :: String,
  windowSize :: Size,
  version :: (Int, Int),
  samples :: Int,
  fullscreen :: Bool,
  vsync :: Bool,
  cursor :: Bool,
  stereo :: Bool,
  debug :: Bool }

appInfo :: APPINFO
appInfo = APPINFO {
  title = "SuperBible6 Example",
  SB6.windowSize  = Size 800 600,
#if DARWIN
  version = (3, 2),
#else
  version = (4, 3),
#endif
  SB6.samples = 0,
  fullscreen  = False,
  vsync  = False,
  SB6.cursor  = True,
  SB6.stereo  = False,
#if DEBUG
  debug  = True
#else
  debug  = False
#endif
  }

run :: Application -> IO ()
run theApp = do
  startTime <- getCurrentTime
  void getArgsAndInitialize
  theAppInfo <- SB6.init theApp

  let numOpt f fld = opt (f . fromIntegral . fld $ theAppInfo) ((> 0) . fld)
      opt val predicate = if predicate theAppInfo then [val] else []
      width (Size w _) = w
      height (Size _ h) = h

  initialDisplayMode $=
    [RGBAMode, WithDepthBuffer, DoubleBuffered] ++
    numOpt WithSamplesPerPixel SB6.samples ++
    opt Stereoscopic SB6.stereo
  initialContextVersion $= version theAppInfo
  initialContextProfile $= [CoreProfile]
  initialContextFlags $= [ForwardCompatibleContext]
#if DEBUG
    ++ [DebugContext]
#endif

  if fullscreen theAppInfo
    then do
      gameModeCapabilities $=
        [Where' GameModeBitsPerPlane IsEqualTo 32] ++
        numOpt (Where' GameModeWidth IsEqualTo) (width . SB6.windowSize) ++
        numOpt (Where' GameModeHeight IsEqualTo) (height . SB6.windowSize)
      void enterGameMode
      windowTitle $= title theAppInfo
    else do
      initialWindowSize $= SB6.windowSize theAppInfo
      void . createWindow . title $ theAppInfo

  unless (SB6.cursor theAppInfo) $
    GLUT.cursor $= None

  displayCallback $= display theApp startTime
  reshapeCallback $= Just (onResize theApp)
  keyboardMouseCallback $= Just (keyboardMouse theApp)
  motionCallback $= Just (onMouseMove theApp)
  passiveMotionCallback $= Just (onMouseMove theApp)

  -- TODO: Setup debug message callback

#if DEBUG
  forM_ [("VENDOR", vendor),
         ("VERSION", glVersion),
         ("RENDERER", renderer)] $ \(name, var) -> do
    val <- get var
    hPutStrLn stderr (name ++ ": " ++ val)
#endif

  startup theApp
  mainLoop
  shutdown theApp
  gma <- get gameModeActive
  when gma leaveGameMode

display :: Application -> UTCTime -> DisplayCallback
display theApp startTime = do
  currentTime <- getCurrentTime
  render theApp $ realToFrac (currentTime `diffUTCTime` startTime)
  swapBuffers
  postRedisplay Nothing

keyboardMouse :: Application -> KeyboardMouseCallback
keyboardMouse theApp key keyState _modifiers _position =
  case key of
    Char c -> onKey theApp (Right c) keyState
    SpecialKey k -> onKey theApp (Left k) keyState
    MouseButton b -> onMouseButton theApp b keyState
