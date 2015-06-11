{-# LANGUAGE CPP #-}
module SB6 (
  Application(..), app,
  AppInfo(..), appInfo,
  run
) where

import Control.Monad ( when, unless, void )
#if DEBUG
import Control.Monad ( forM_ )
#endif
import Data.List ( isPrefixOf )
import Data.Time.Clock ( UTCTime, diffUTCTime, getCurrentTime )
import Foreign.C.Types
import Foreign.Ptr ( FunPtr, nullFunPtr )
import System.Exit ( exitSuccess )
import System.IO ( hPutStrLn, stderr )

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
  , onDebugMessage :: DebugMessage -> IO ()
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
  , onDebugMessage =
      \(DebugMessage _source _typ _ident _severity message) ->
        hPutStrLn stderr message
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
#if OS_DARWIN
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
  swapInterval $ if vsync theAppInfo then 1 else 0
  displayCallback $= displayCB theApp startTime
  closeCallback $= Just (closeCB theApp)
  reshapeCallback $= Just (onResize theApp)
  keyboardMouseCallback $= Just (keyboardMouseCB theApp)
  motionCallback $= Just (onMouseMove theApp)
  passiveMotionCallback $= Just (onMouseMove theApp)

  when (debug theAppInfo) $ do
    debugMessageCallback $= Just (onDebugMessage theApp)
    debugOutputSynchronous $= Enabled

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

displayCB :: Application -> UTCTime -> DisplayCallback
displayCB theApp startTime = do
  currentTime <- getCurrentTime
  render theApp $ realToFrac (currentTime `diffUTCTime` startTime)
  swapBuffers
  postRedisplay Nothing

keyboardMouseCB :: Application -> KeyboardMouseCallback
keyboardMouseCB theApp key keyState _modifiers _position =
  case (key, keyState) of
    (Char '\ESC', Up) -> do
      closeCB theApp
    (Char c, _) -> onKey theApp (Right c) keyState
    (SpecialKey k, _) -> onKey theApp (Left k) keyState
    (MouseButton b, _) -> onMouseButton theApp b keyState

closeCB :: Application -> IO ()
closeCB theApp = do
  shutdown theApp
  gma <- get gameModeActive
  when gma leaveGameMode
  displayCallback $= return ()
  closeCallback $= Nothing
  ifFreeGLUT leaveMainLoop exitSuccess

ifFreeGLUT :: IO () -> IO () -> IO ()
ifFreeGLUT freeGLUTAction otherAction = do
  v <- get glutVersion
  if "freeglut" `isPrefixOf` v
    then freeGLUTAction
    else otherAction

--------------------------------------------------------------------------------

-- Note that the list of extensions might be empty because we use the core
-- profile, so we can't test the existence before the actual getProcAddress.
swapInterval :: Int -> IO ()
swapInterval interval = do
  funPtr <- getProcAddress swapIntervalName
  unless (funPtr == nullFunPtr) $
    void $ makeSwapInterval funPtr (fromIntegral interval)

swapIntervalName :: String
#if OS_WINDOWS
swapIntervalName = "wglGetSwapIntervalEXT"

foreign import CALLCONV "dynamic" makeSwapInterval
  :: FunPtr (CInt -> IO CInt)
  ->         CInt -> IO CInt
#else
swapIntervalName = "glXSwapIntervalSGI"

foreign import CALLCONV "dynamic" makeSwapInterval
  :: FunPtr (CInt -> IO CInt)
  ->         CInt -> IO CInt
#endif
