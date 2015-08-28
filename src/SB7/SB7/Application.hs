{-# LANGUAGE CPP #-}
module SB7.Application (
  Application(..), app,
  AppInfo(..), appInfo,
  run,
  windowTitle, extensionSupported
) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (<$>) )
#endif
import Control.Monad ( forM_, when, unless, void )
import Data.List ( isPrefixOf )
import Data.Time.Clock ( UTCTime, diffUTCTime, getCurrentTime )
import Foreign.C.Types
import Foreign.Ptr ( FunPtr, nullFunPtr )
import System.Exit ( exitSuccess )
import System.IO ( hPutStrLn, stderr )
import System.Info ( os )

import Graphics.UI.GLUT as GLUT
import Graphics.Rendering.OpenGL.Raw ( getProcAddress )

--------------------------------------------------------------------------------

-- Note: We provide no onMouseWheel callback, because the underlying API is
-- implemented in freeglut only, not in classic GLUT. Furthermore, onMouseButton
-- gets called with WheelUp/WheelDown as the button, so no functionality is
-- missing. And finally: No example from the book is using onMouseWheel.
--
-- There is no getMousePosition function, either, because neither GLUT nor
-- freeglut directly provide an API for this. Furthermore, this can easily be
-- emulated via an IORef in the application state holding the current mouse
-- position which gets updated via onMouseMove.
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
  { SB7.Application.init = return appInfo
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

-- Note: We provide no "robust" flag, because currently neither GLUT nor
-- freeglut provide an API for the ARB_create_context_robustness extension. And
-- no example from the book is using it, anyway.
data AppInfo = AppInfo
  { title :: String
  , windowSize :: Size
  , version :: (Int, Int)
  , numSamples :: Int  -- renamed from 'samples' to avoid a clash with OpenGL
  , fullscreen :: Bool
  , vsync :: Bool
  , cursor :: Bool
  , stereo :: Bool
  , debug :: Bool
  } deriving ( Eq, Ord, Show )

appInfo :: AppInfo
appInfo = AppInfo
  { title = "OpenGL SuperBible Example"
  ,  SB7.Application.windowSize  = Size 800 600
  , version = if os `elem` [ "darwin", "osx" ] then (3, 2) else (4, 3)
  , numSamples = 0
  , fullscreen  = False
  , vsync  = False
  , SB7.Application.cursor  = True
  , SB7.Application.stereo  = False
  , debug  = False
  }

--------------------------------------------------------------------------------

run :: Application s -> IO ()
run theApp = do
  startTime <- getCurrentTime
  (_progName, args) <- getArgsAndInitialize
  theAppInfo <- handleArgs args <$> SB7.Application.init theApp
  let numOpt f fld = opt (f . fromIntegral . fld $ theAppInfo) ((> 0) . fld)
      opt val predicate = if predicate theAppInfo then [ val ] else []
      width (Size w _) = w
      height (Size _ h) = h
  initialDisplayMode $=
    [ RGBAMode, WithDepthBuffer, DoubleBuffered ] ++
    numOpt WithSamplesPerPixel numSamples ++
    opt Stereoscopic SB7.Application.stereo
  initialContextVersion $= version theAppInfo
  initialContextProfile $= [ CoreProfile ]
  initialContextFlags $= [ ForwardCompatibleContext ] ++ opt DebugContext debug
  if fullscreen theAppInfo
    then do
      gameModeCapabilities $=
        [ Where' GameModeBitsPerPlane IsEqualTo 32 ] ++
        numOpt (Where' GameModeWidth IsEqualTo) (width . SB7.Application.windowSize) ++
        numOpt (Where' GameModeHeight IsEqualTo) (height . SB7.Application.windowSize)
      void enterGameMode
      windowTitle $= title theAppInfo
    else do
      initialWindowSize $= SB7.Application.windowSize theAppInfo
      void . createWindow . title $ theAppInfo
  unless (SB7.Application.cursor theAppInfo) (GLUT.cursor $= None)
  swapInterval $ if vsync theAppInfo then 1 else 0

  when (debug theAppInfo) $
    forM_ [ ("VENDOR", vendor),
            ("VERSION", glVersion),
            ("RENDERER", renderer) ] $ \(name, var) -> do
      val <- get var
      hPutStrLn stderr (name ++ ": " ++ val)

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

handleArgs :: [String] -> AppInfo -> AppInfo
handleArgs args theAppInfo = foldr foo theAppInfo args
  where foo :: String -> AppInfo -> AppInfo
        foo arg ai
          | arg == "-nofullscreen" = ai { fullscreen = False }
          | arg == "-fullscreen" = ai { fullscreen = True }
          | arg == "-novsync" = ai { vsync = False }
          | arg == "-vsync" = ai { vsync = True }
          | arg == "-nocursor" = ai { SB7.Application.cursor = False }
          | arg == "-cursor" = ai { SB7.Application.cursor = True }
          | arg == "-nostereo" = ai { SB7.Application.stereo = False }
          | arg == "-stereo" = ai { SB7.Application.stereo = True }
          | arg == "-nodebug" = ai { debug = False }
          | arg == "-debug" = ai { debug = True }
          | otherwise = ai

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
  -- Exiting is a bit tricky due to a freeglut bug: leaveMainLoop just sets a
  -- flag that the next iteration of the main loop should exit, but the current
  -- iteration will handle all events first and then go to sleep until there is
  -- something to do. This means that a simple leaveMainLoop alone won't work,
  -- even if we add some work which can be done immediately. So as a workaround,
  -- we register a timer callback which never gets called (but we nevertheless
  -- have to sleep for that time). Ugly!
  ifFreeGLUT (do leaveMainLoop; addTimerCallback 10 (return ())) exitSuccess

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
