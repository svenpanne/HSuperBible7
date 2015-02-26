{-# LANGUAGE CPP #-}
module SB6 (
  Application(..), app,
  AppInfo(..), appInfo,
  run,
  DebugSource(..), DebugType(..), DebugSeverity(..),
  packUtf8
) where

import Control.Monad ( when, unless, void )
#if DEBUG
import Control.Monad ( forM_ )
#endif
import qualified Data.ByteString as B
import Data.List ( isPrefixOf )
import Data.Time.Clock ( UTCTime, diffUTCTime, getCurrentTime )
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Foreign.C.String ( peekCStringLen )
import Foreign.C.Types
import Foreign.Ptr ( nullPtr, FunPtr, nullFunPtr )
import System.Exit ( exitSuccess )
import System.IO ( hPutStrLn, stderr )

import Graphics.UI.GLUT as GLUT
import Graphics.Rendering.OpenGL.Raw

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
  , onDebugMessage ::
      DebugSource -> DebugType -> Integer -> DebugSeverity -> String -> IO ()
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
      \_source _typ _ident _severity message -> hPutStrLn stderr message
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
    v <- get (majorMinor glVersion)
    if v >= (4, 3)
      then do
        cb <- makeGLDEBUGPROC (debugCB theApp)
        glDebugMessageCallback cb nullPtr
        glEnable gl_DEBUG_OUTPUT_SYNCHRONOUS
      else do
        funPtr <- getProcAddress "glDebugMessageCallbackARB"
        unless (funPtr == nullFunPtr) $ do
          cb <- makeGLDEBUGPROC (debugCB theApp)
          glDebugMessageCallbackARB cb nullPtr
          glEnable gl_DEBUG_OUTPUT_SYNCHRONOUS_ARB

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

debugCB :: Application -> GLDEBUGPROCFunc
debugCB theApp source typ ident severity len message _userParam = do
  msg <- peekCStringLen (message, fromIntegral len)
  onDebugMessage theApp
    (unmarshalDebugSource source)
    (unmarshalDebugType typ)
    (fromIntegral ident)
    (unmarshalDebugSeverity severity)
    msg

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

--------------------------------------------------------------------------------

data DebugSource =
    DebugSourceAPI
  | DebugSourceWindowSystem
  | DebugSourceShaderCompiler
  | DebugSourceThirdParty
  | DebugSourceApplication
  | DebugSourceOther
  deriving ( Eq, Ord, Show )

unmarshalDebugSource :: GLenum -> DebugSource
unmarshalDebugSource x
  | x == gl_DEBUG_SOURCE_API = DebugSourceAPI
  | x == gl_DEBUG_SOURCE_WINDOW_SYSTEM = DebugSourceWindowSystem
  | x == gl_DEBUG_SOURCE_SHADER_COMPILER = DebugSourceShaderCompiler
  | x == gl_DEBUG_SOURCE_THIRD_PARTY = DebugSourceThirdParty
  | x == gl_DEBUG_SOURCE_APPLICATION = DebugSourceApplication
  | x == gl_DEBUG_SOURCE_OTHER = DebugSourceOther
  | otherwise = error ("unmarshalDebugSource: illegal value " ++ show x)

--------------------------------------------------------------------------------

data DebugType =
    DebugTypeError
  | DebugTypeDeprecatedBehavior
  | DebugTypeUndefinedBehavior
  | DebugTypePortability
  | DebugTypePerformance
  | DebugTypeOther
  | DebugTypeMarker
  | DebugTypePushGroup
  | DebugTypePopGroup
  deriving ( Eq, Ord, Show )

unmarshalDebugType :: GLenum -> DebugType
unmarshalDebugType x
  | x == gl_DEBUG_TYPE_ERROR = DebugTypeError
  | x == gl_DEBUG_TYPE_DEPRECATED_BEHAVIOR = DebugTypeDeprecatedBehavior
  | x == gl_DEBUG_TYPE_UNDEFINED_BEHAVIOR = DebugTypeUndefinedBehavior
  | x == gl_DEBUG_TYPE_PORTABILITY = DebugTypePortability
  | x == gl_DEBUG_TYPE_PERFORMANCE = DebugTypePerformance
  | x == gl_DEBUG_TYPE_OTHER = DebugTypeOther
  | x == gl_DEBUG_TYPE_MARKER = DebugTypeMarker
  | x == gl_DEBUG_TYPE_PUSH_GROUP = DebugTypePushGroup
  | x == gl_DEBUG_TYPE_POP_GROUP = DebugTypePopGroup
  | otherwise = error ("unmarshalDebugType: illegal value " ++ show x)

--------------------------------------------------------------------------------

data DebugSeverity =
    DebugSeverityHigh
  | DebugSeverityMedium
  | DebugSeverityLow
  | DebugSeverityNotification
  deriving ( Eq, Ord, Show )

unmarshalDebugSeverity :: GLenum -> DebugSeverity
unmarshalDebugSeverity x
  | x == gl_DEBUG_SEVERITY_HIGH = DebugSeverityHigh
  | x == gl_DEBUG_SEVERITY_MEDIUM = DebugSeverityMedium
  | x == gl_DEBUG_SEVERITY_LOW = DebugSeverityLow
  | x == gl_DEBUG_SEVERITY_NOTIFICATION = DebugSeverityNotification
  | otherwise = error ("unmarshalDebugSeverity: illegal value " ++ show x)

--------------------------------------------------------------------------------

packUtf8 :: String -> B.ByteString
packUtf8 = TE.encodeUtf8 . T.pack
