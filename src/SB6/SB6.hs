{-# LANGUAGE CPP #-}
module SB6 (
  Application(..), app,
  AppInfo(..), appInfo,
  run
) where

import Control.Monad
import Data.List
import Data.Time.Clock
import Foreign.C.Types
import Foreign.Ptr
import System.Exit
#if DEBUG
import System.IO
#endif
import Graphics.UI.GLUT as GLUT
import Graphics.Rendering.OpenGL.Raw ( getProcAddress )

--------------------------------------------------------------------------------

data Application = Application
  { init :: IO AppInfo
  , startup :: IO ()
  , render :: Double -> IO ()
  , shutdown :: IO ()
  , onResize :: Size -> IO ()
  , onKey :: Either SpecialKey Char -> KeyState -> IO ()
  , onMouseButton :: MouseButton -> KeyState -> IO ()
  , onMouseMove :: Position -> IO ()
  }

app :: Application
app = Application
  { SB6.init = return appInfo
  , startup = return ()
  , render = \_currentTime -> return ()
  , shutdown = return ()
  , onResize = \_size -> return ()
  , onKey = \_key _keyState -> return ()
  , onMouseButton = \_mouseButton _keyState -> return ()
  , onMouseMove = \_position -> return ()
  }

--------------------------------------------------------------------------------

data AppInfo = AppInfo
  { title :: String
  , windowSize :: Size
  , version :: (Int, Int)
  , samples :: Int
  , fullscreen :: Bool
  , vsync :: Bool
  , cursor :: Bool
  , stereo :: Bool
  , debug :: Bool
  }

appInfo :: AppInfo
appInfo = AppInfo
  { title = "SuperBible6 Example"
  ,  SB6.windowSize  = Size 800 600
#if DARWIN
  , version = (3, 2)
#else
  , version = (4, 3)
#endif
  , SB6.samples = 0
  , fullscreen  = False
  , vsync  = False
  , SB6.cursor  = True
  , SB6.stereo  = False
#if DEBUG
  , debug  = True
#else
  , debug  = False
#endif
  }

--------------------------------------------------------------------------------

run :: Application -> IO ()
run theApp = do
  startTime <- getCurrentTime
  void getArgsAndInitialize
  theAppInfo <- SB6.init theApp
  let numOpt f fld = opt (f . fromIntegral . fld $ theAppInfo) ((> 0) . fld)
      opt val predicate = if predicate theAppInfo then [ val ] else []
      width (Size w _) = w
      height (Size _ h) = h
  initialDisplayMode $=
    [ RGBAMode, WithDepthBuffer, DoubleBuffered ] ++
    numOpt WithSamplesPerPixel SB6.samples ++
    opt Stereoscopic SB6.stereo
  initialContextVersion $= version theAppInfo
  initialContextProfile $= [ CoreProfile ]
  initialContextFlags $= [ ForwardCompatibleContext ]
#if DEBUG
    ++ [ DebugContext ]
#endif
  if fullscreen theAppInfo
    then do
      gameModeCapabilities $=
        [ Where' GameModeBitsPerPlane IsEqualTo 32 ] ++
        numOpt (Where' GameModeWidth IsEqualTo) (width . SB6.windowSize) ++
        numOpt (Where' GameModeHeight IsEqualTo) (height . SB6.windowSize)
      void enterGameMode
      windowTitle $= title theAppInfo
    else do
      initialWindowSize $= SB6.windowSize theAppInfo
      void . createWindow . title $ theAppInfo
  unless (SB6.cursor theAppInfo) (GLUT.cursor $= None)
  when (vsync theAppInfo) (swapInterval 1)
  displayCallback $= display theApp startTime
  closeCallback $= Just (terminate theApp)
  reshapeCallback $= Just (onResize theApp)
  keyboardMouseCallback $= Just (keyboardMouse theApp)
  motionCallback $= Just (onMouseMove theApp)
  passiveMotionCallback $= Just (onMouseMove theApp)

  -- TODO: Setup debug message callback
#if DEBUG
  forM_ [ ("VENDOR", vendor),
          ("VERSION", glVersion),
          ("RENDERER", renderer) ] $ \(name, var) -> do
    val <- get var
    hPutStrLn stderr (name ++ ": " ++ val)
#endif
  startup theApp
  ifFreeGLUT (actionOnWindowClose $= MainLoopReturns) (return ())
  mainLoop

display :: Application -> UTCTime -> DisplayCallback
display theApp startTime = do
  currentTime <- getCurrentTime
  render theApp $ realToFrac (currentTime `diffUTCTime` startTime)
  swapBuffers
  postRedisplay Nothing

keyboardMouse :: Application -> KeyboardMouseCallback
keyboardMouse theApp key keyState _modifiers _position =
  case (key, keyState) of
    (Char '\ESC', Up) -> do closeCallback $= Nothing; terminate theApp
    (Char c, _) -> onKey theApp (Right c) keyState
    (SpecialKey k, _) -> onKey theApp (Left k) keyState
    (MouseButton b, _) -> onMouseButton theApp b keyState

terminate :: Application -> IO ()
terminate theApp = do
  shutdown theApp
  gma <- get gameModeActive
  when gma leaveGameMode
  ifFreeGLUT leaveMainLoop exitSuccess

ifFreeGLUT :: IO () -> IO () -> IO ()
ifFreeGLUT freeGLUTAction otherAction = do
  version <- get glutVersion
  if "freeglut" `isPrefixOf` version
    then freeGLUTAction
    else otherAction

--------------------------------------------------------------------------------

-- Note that the list of extensions might be empty because we use the core
-- profile, so we can't test the existence before the actual getProcAddress.
swapInterval :: Int -> IO ()
#if 0
swapInterval interval = do
  found <- tryCall makeWglSwapInterval "wglGetSwapIntervalEXT" interval
  unless found $
    void $ tryCall makeGlXSwapInterval "glXSwapIntervalSGI" interval

type SwapInterval = CInt -> IO CInt

tryCall :: (FunPtr (SwapInterval) -> SwapInterval) -> String -> Int -> IO Bool
tryCall make name interval = do
  p <- getProcAddress name
  if p == nullFunPtr
    then return False
    else make p (fromIntegral interval) >> return True

foreign import stdcall "dynamic" makeWglSwapInterval
  :: FunPtr SwapInterval -> SwapInterval

foreign import ccall "dynamic" makeGlXSwapInterval
  :: FunPtr SwapInterval -> SwapInterval
#endif


#if 0
swapInterval interval = do
  funPtr <- getProcAddress "glXSwapIntervalSGI"
  unless (funPtr == nullFunPtr) $
    void $ dyn_glXSwapIntervalSGI funPtr (fromIntegral interval)

foreign import ccall "dynamic" dyn_glXSwapIntervalSGI
  :: FunPtr (CInt -> IO CInt)
  ->         CInt -> IO CInt

#elif 1
swapInterval interval = do
  funPtr <- getProcAddress "wglGetSwapIntervalEXT"
  unless (funPtr == nullFunPtr) $
    void $ dyn_wglGetSwapIntervalEXT funPtr (fromIntegral interval)

type BOOL = CInt

foreign import stdcall "dynamic" dyn_wglGetSwapIntervalEXT
  :: FunPtr (CInt -> IO BOOL)
  ->         CInt -> IO BOOL

#else

#endif
