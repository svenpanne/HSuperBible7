{-# LANGUAGE CPP #-}
module SB6.Application (
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

data Application s = Application
  { init :: IO AppInfo
  , startup :: IO s
  , render :: s -> Double -> IO ()
  , shutdown :: s -> IO ()
  , onResize :: s -> Size -> IO ()
  , onKey :: s -> Either SpecialKey Char -> KeyState -> IO ()
  , onMouseButton :: s -> MouseButton -> KeyState -> IO ()
  , onMouseMove :: s -> Position -> IO ()
  , onDebugMessage :: s -> DebugMessage -> IO ()
  }

app :: Application s
app = Application
  { SB6.Application.init = return appInfo
  , startup = return undefined
  , render = \_state _currentTime -> return ()
  , shutdown = \_state -> return ()
  , onResize = \_state _size -> return ()
  , onKey = \_state _key _keyState -> return ()
  , onMouseButton = \_state _mouseButton _keyState -> return ()
  , onMouseMove = \_state _position -> return ()
  , onDebugMessage =
      \_state (DebugMessage _source _typ _ident _severity message) ->
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
  } deriving ( Eq, Ord, Show )

appInfo :: AppInfo
appInfo = AppInfo
  { title = "SuperBible6 Example"
  ,  SB6.Application.windowSize  = Size 800 600
#if OS_DARWIN
  , version = (3, 2)
#else
  , version = (4, 3)
#endif
  , SB6.Application.samples = 0
  , fullscreen  = False
  , vsync  = False
  , SB6.Application.cursor  = True
  , SB6.Application.stereo  = False
#if DEBUG
  , debug  = True
#else
  , debug  = False
#endif
  }

--------------------------------------------------------------------------------

run :: Application s -> IO ()
run theApp = do
  startTime <- getCurrentTime
  void getArgsAndInitialize
  theAppInfo <- SB6.Application.init theApp
  let numOpt f fld = opt (f . fromIntegral . fld $ theAppInfo) ((> 0) . fld)
      opt val predicate = if predicate theAppInfo then [ val ] else []
      width (Size w _) = w
      height (Size _ h) = h
  initialDisplayMode $=
    [ RGBAMode, WithDepthBuffer, DoubleBuffered ] ++
    numOpt WithSamplesPerPixel SB6.Application.samples ++
    opt Stereoscopic SB6.Application.stereo
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
        numOpt (Where' GameModeWidth IsEqualTo) (width . SB6.Application.windowSize) ++
        numOpt (Where' GameModeHeight IsEqualTo) (height . SB6.Application.windowSize)
      void enterGameMode
      windowTitle $= title theAppInfo
    else do
      initialWindowSize $= SB6.Application.windowSize theAppInfo
      void . createWindow . title $ theAppInfo
  unless (SB6.Application.cursor theAppInfo) (GLUT.cursor $= None)
  swapInterval $ if vsync theAppInfo then 1 else 0

#if DEBUG
  forM_ [ ("VENDOR", vendor),
          ("VERSION", glVersion),
          ("RENDERER", renderer) ] $ \(name, var) -> do
    val <- get var
    hPutStrLn stderr (name ++ ": " ++ val)
#endif
  state <- startup theApp

  displayCallback $= displayCB theApp state startTime
  closeCallback $= Just (closeCB theApp state)
  reshapeCallback $= Just (onResize theApp state)
  keyboardMouseCallback $= Just (keyboardMouseCB theApp state)
  motionCallback $= Just (onMouseMove theApp state)
  passiveMotionCallback $= Just (onMouseMove theApp state)
  when (debug theAppInfo) $ do
    debugMessageCallback $= Just (onDebugMessage theApp state)
    debugOutputSynchronous $= Enabled

  ifFreeGLUT (actionOnWindowClose $= MainLoopReturns) (return ())
  mainLoop

displayCB :: Application s -> s -> UTCTime -> DisplayCallback
displayCB theApp state startTime = do
  currentTime <- getCurrentTime
  render theApp state $ realToFrac (currentTime `diffUTCTime` startTime)
  swapBuffers
  postRedisplay Nothing

keyboardMouseCB :: Application s -> s -> KeyboardMouseCallback
keyboardMouseCB theApp state key keyState _modifiers _position =
  case (key, keyState) of
    (Char '\ESC', Up) -> closeCB theApp state
    (Char c, _) -> onKey theApp state (Right c) keyState
    (SpecialKey k, _) -> onKey theApp state (Left k) keyState
    (MouseButton b, _) -> onMouseButton theApp state b keyState

closeCB :: Application s -> s -> IO ()
closeCB theApp state = do
  shutdown theApp state
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
