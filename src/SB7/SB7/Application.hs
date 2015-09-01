{-# LANGUAGE CPP #-}
module SB7.Application (
  Application(..), app,
  AppInfo(..), appInfo,
  run,
  windowTitle, windowSize, extensionSupported,
  SpecialKey(..), KeyState(..), MouseButton(..)
) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (<$>) )
#endif
import Control.Monad ( forM_, when, unless, void )
import Control.Monad.Trans.Reader ( local )
import Data.IORef ( newIORef )
import Data.List ( isPrefixOf )
import Data.Time.Clock ( UTCTime, diffUTCTime, getCurrentTime )
import Foreign.C.Types
import Foreign.Ptr ( FunPtr, nullFunPtr )
import Options.Applicative
import Options.Applicative.Types ( ReadM(..), readerAsk )
import System.Exit ( exitSuccess )
import System.IO ( hPutStrLn, stderr )
import System.Info ( os )

import Graphics.UI.GLUT as GLUT
import Graphics.Rendering.OpenGL.Raw ( getProcAddress )

--------------------------------------------------------------------------------

#if !MIN_VERSION_base(4,5,0)
import Data.Monoid ( Monoid )

infixr 6 <>

-- | An infix synonym for 'mappend'.
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}
#endif

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
  , onResize :: s -> Size -> IO s
  , onKey :: s -> Either SpecialKey Char -> KeyState -> IO s
  , onMouseButton :: s -> MouseButton -> KeyState -> IO s
  , onMouseMove :: s -> Position -> IO s
  , onDebugMessage :: s -> DebugMessage -> IO ()
  }

app :: Application s
app = Application
  { SB7.Application.init = return appInfo
  , startup = return undefined
  , render = \_state _currentTime -> return ()
  , shutdown = \_state -> return ()
  , onResize = \state _size -> return state
  , onKey = \state _key _keyState -> return state
  , onMouseButton = \state _mouseButton _keyState -> return state
  , onMouseMove = \state _position -> return state
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
  , initialWindowSize :: Size
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
  , SB7.Application.initialWindowSize  = Size 800 600
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
  (_progName, args) <- getArgsAndInitialize
  theAppInfo <- handleArgs args =<< SB7.Application.init theApp
  initializeGLUT theAppInfo
  printInfo theAppInfo
  theState <- startup theApp
  registerCallbacks theApp theAppInfo theState
  ifFreeGLUT (actionOnWindowClose $= MainLoopReturns) (return ())
  mainLoop

initializeGLUT :: AppInfo -> IO ()
initializeGLUT theAppInfo = do
  let numOpt f fld = opt (f . fromIntegral . fld $ theAppInfo) ((> 0) . fld)
      opt val predicate = if predicate theAppInfo then [ val ] else []
      width = (\(Size w _) -> w) . SB7.Application.initialWindowSize
      height = (\(Size _ h) -> h) . SB7.Application.initialWindowSize
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
        numOpt (Where' GameModeWidth IsEqualTo) width ++
        numOpt (Where' GameModeHeight IsEqualTo) height
      void enterGameMode
      windowTitle $= title theAppInfo
    else do
      GLUT.initialWindowSize $= SB7.Application.initialWindowSize theAppInfo
      void . createWindow . title $ theAppInfo
  unless (SB7.Application.cursor theAppInfo) (GLUT.cursor $= None)
  swapInterval $ if vsync theAppInfo then 1 else 0

printInfo :: AppInfo -> IO ()
printInfo theAppInfo = do
  when (debug theAppInfo) $
    forM_ [ ("VENDOR", vendor),
            ("VERSION", glVersion),
            ("RENDERER", renderer) ] $ \(name, var) -> do
      val <- get var
      hPutStrLn stderr (name ++ ": " ++ val)

registerCallbacks :: Application s -> AppInfo -> s -> IO ()
registerCallbacks theApp theAppInfo theState = do
  state <- newIORef theState
  let updateState cb = get state >>= cb >>= (state $=)
  startTime <- getCurrentTime
  displayCallback $= (updateState $ displayCB theApp startTime)
  closeCallback $= Just (updateState $ closeCB theApp)
  reshapeCallback $= Just (\size -> updateState $ \s -> onResize theApp s size)
  keyboardMouseCallback $= Just (\key keyState _modifiers _position ->
    case (key, keyState) of
      (Char '\ESC', Up) -> updateState $ closeCB theApp
      (Char c, _) -> updateState $ \s -> onKey theApp s (Right c) keyState
      (SpecialKey k, _) -> updateState $ \s -> onKey theApp s (Left k) keyState
      (MouseButton b, _) ->
        updateState $ \s -> onMouseButton theApp s b keyState)
  motionCallback $= Just (\pos -> updateState $ \s -> onMouseMove theApp s pos)
  passiveMotionCallback $=
    Just (\pos -> updateState $ \s -> onMouseMove theApp s pos)
  when (debug theAppInfo) $ do
    debugMessageCallback $=
      Just (\msg -> updateState $ \s -> onDebugMessage theApp s msg >> return s)
    debugOutputSynchronous $= Enabled

displayCB :: Application s -> UTCTime -> s -> IO s
displayCB theApp startTime state = do
  currentTime <- getCurrentTime
  render theApp state $ realToFrac (currentTime `diffUTCTime` startTime)
  swapBuffers
  postRedisplay Nothing
  return state

closeCB :: Application s -> s -> IO s
closeCB theApp state = do
  shutdown theApp state
  displayCallback $= return ()
  closeCallback $= Nothing
  gma <- get gameModeActive
  when gma leaveGameMode
  -- Exiting is a bit tricky due to a freeglut bug: leaveMainLoop just sets a
  -- flag that the next iteration of the main loop should exit, but the current
  -- iteration will handle all events first and then go to sleep until there is
  -- something to do. This means that a simple leaveMainLoop alone won't work,
  -- even if we add some work which can be done immediately. So as a workaround,
  -- we register a timer callback which never gets called (but we nevertheless
  -- have to sleep for that time). Ugly!
  ifFreeGLUT (do leaveMainLoop; addTimerCallback 10 (return ())) exitSuccess
  return state

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

--------------------------------------------------------------------------------
-- Commandline handling: Not in the original code, but very convenient.

handleArgs :: [String] -> AppInfo -> IO AppInfo
handleArgs args theAppInfo =
  handleParseResult $ execParserPure (prefs idm) opts args
  where opts = info (helper <*> parseWith theAppInfo) fullDesc

parseWith :: AppInfo -> Parser AppInfo
parseWith theAppInfo = AppInfo
  <$> strOption (long "title"
              <> metavar "TITLE"
              <> defaultValueWith show title
              <> help "Set window title")
  <*> option (pair Size nonNegative 'x' nonNegative)
             (long "initial-window-size"
           <> metavar "WxH"
           <> defaultValueWith showSize SB7.Application.initialWindowSize
           <> help "Set initial window size")
  <*> option (pair (,) nonNegative '.' nonNegative)
             (long "version"
           <> metavar "MAJOR.MINOR"
           <> defaultValueWith showVersion version
           <> help "Set OpenGL version to use")
  <*> option nonNegative (long "num-samples"
                       <> metavar "N"
                       <> defaultValueWith show numSamples
                       <> help "Control multisampling, 0 = none")
  <*> boolOption "fullscreen" fullscreen "full screen mode"
  <*> boolOption "vsync" vsync "vertical synchronization"
  <*> boolOption "cursor" SB7.Application.cursor "cursor"
  <*> boolOption "stereo" SB7.Application.stereo "stereoscopic mode"
  <*> boolOption "debug" debug "debugging features"
  where defaultValueWith s proj = value (proj theAppInfo) <> showDefaultWith s
        showSize (Size w h) = show w ++ "x" ++ show h
        showVersion (major, minor) = show major ++ "." ++ show minor
        boolOption longName proj what =
          option boolean (long longName
                       <> metavar "BOOL"
                       <> defaultValueWith show proj
                       <> help ("Enable " ++ what))

pair :: (a -> b -> c) -> ReadM a -> Char -> ReadM b -> ReadM c
pair p r1 sep r2 = do
  s <- readerAsk
  case break (== sep) s of
    (x, (_:y)) -> p <$> localM (const x) r1 <*> localM (const y) r2
    _ -> readerError $ "missing separator " ++ show [sep]
  where localM f = ReadM . local f . unReadM

nonNegative :: (Read a, Show a, Integral a) => ReadM a
nonNegative = do
  s <- readerAsk
  case reads s of
    [(i, "")]
      | i >= 0 -> return i
      | otherwise -> readerError $ show i ++ " is negative"
    _ -> readerError $ show s ++ " is not an integer"

boolean :: ReadM Bool
boolean = do
  s <- readerAsk
  case () of
    _ | s `elem` [ "0", "f", "F", "false", "FALSE", "False" ] ->  return False
      | s `elem` [ "1", "t", "T", "true", "TRUE", "True" ] -> return True
      | otherwise -> readerError $ show s ++ " is not a boolean"
