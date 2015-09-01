-- FragColorFromPos, see listings 3.10 - 3.12 in the OpenGL SuperBible, 7th ed.
-- Adapted from fragcolorfrompos.cpp which is (c) 2012-2015 Graham Sellers.

module Main ( main ) where

import SB7

-- Note that we deviate a bit from the book: We use a key callback to toggle
-- (via the 'm' key) between the 2 shader programs instead of making this a
-- compile-time decision.
data State = State
  { interpolate :: Bool
  , programPos :: Program
  , programInterpolate :: Program
  , vao :: VertexArrayObject
  }

init :: IO AppInfo
init = return $ appInfo { title = "OpenGL SuperBible - Simple Triangle" }

startup :: IO State
startup = do
  let vs_source_pos = unlines
        [ "#version 420 core                                                          "
        , "                                                                           "
        , "void main(void)                                                            "
        , "{                                                                          "
        , "    const vec4 vertices[] = vec4[](vec4( 0.25, -0.25, 0.5, 1.0),           "
        , "                                   vec4(-0.25, -0.25, 0.5, 1.0),           "
        , "                                   vec4( 0.25,  0.25, 0.5, 1.0));          "
        , "                                                                           "
        , "    gl_Position = vertices[gl_VertexID];                                   "
        , "}                                                                          " ]
      fs_source_pos = unlines
        [ "#version 420 core                                                          "
        , "                                                                           "
        , "out vec4 color;                                                            "
        , "                                                                           "
        , "void main(void)                                                            "
        , "{                                                                          "
        , "    color = vec4(sin(gl_FragCoord.x * 0.25) * 0.5 + 0.5,                   "
        , "                 cos(gl_FragCoord.y * 0.25) * 0.5 + 0.5,                   "
        , "                 sin(gl_FragCoord.x * 0.15) * cos(gl_FragCoord.y * 0.1),   "
        , "                 1.0);                                                     "
        , "}                                                                          " ]
      vs_source_interpolate = unlines
        [ "#version 420 core                                                          "
        , "                                                                           "
        , "out vec4 vs_color;                                                         "
        , "void main(void)                                                            "
        , "{                                                                          "
        , "    const vec4 vertices[] = vec4[](vec4( 0.25, -0.25, 0.5, 1.0),           "
        , "                                   vec4(-0.25, -0.25, 0.5, 1.0),           "
        , "                                   vec4( 0.25,  0.25, 0.5, 1.0));          "
        , "    const vec4 colors[] = vec4[](vec4(1.0, 0.0, 0.0, 1.0),                 "
        , "                                 vec4(0.0, 1.0, 0.0, 1.0),                 "
        , "                                 vec4(0.0, 0.0, 1.0, 1.0));                "
        , "                                                                           "
        , "    gl_Position = vertices[gl_VertexID];                                   "
        , "    vs_color = colors[gl_VertexID];                                        "
        , "}                                                                          " ]
      fs_source_interpolate = unlines
        [ "#version 420 core                                                          "
        , "                                                                           "
        , "in vec4 vs_color;                                                          "
        , "out vec4 color;                                                            "
        , "                                                                           "
        , "void main(void)                                                            "
        , "{                                                                          "
        , "    color = vs_color;                                                      "
        , "}                                                                          " ]

  theProgramInterpolate <-
    compileAndLink fs_source_interpolate vs_source_interpolate
  theProgramPos <- compileAndLink fs_source_pos vs_source_pos

  theVao <- genObjectName
  bindVertexArrayObject $= Just theVao

  return $ State
    { interpolate = True
    , programPos = theProgramPos
    , programInterpolate = theProgramInterpolate
    , vao = theVao
    }

compileAndLink :: String -> String -> IO Program
compileAndLink fs_source vs_source = do
  theProgram <- createProgram

  fs <- createShader FragmentShader
  shaderSourceBS fs $= packUtf8 fs_source
  compileShader fs

  vs <- createShader VertexShader
  shaderSourceBS vs $= packUtf8 vs_source
  compileShader vs

  mapM_ (attachShader theProgram) [ vs, fs ]
  linkProgram theProgram
  return theProgram

onKey :: State -> Either SpecialKey Char -> KeyState -> IO State
onKey state (Right 'm') Down =
  return state { interpolate = not (interpolate state) }
onKey state _ _ = return state

render :: State -> Double -> IO ()
render state _currentTime = do
  clearBuffer $ ClearColorBufferFloat 0 (Color4 0 0.25 0 1)

  let prog | interpolate state = programInterpolate state
           | otherwise = programPos state
  currentProgram $= Just prog
  drawArrays Triangles 0 3

shutdown :: State -> IO ()
shutdown state = do
  deleteObjectName $ vao state
  deleteObjectName $ programPos state
  deleteObjectName $ programInterpolate state

main :: IO ()
main = run $ app
  { SB7.init = Main.init
  , SB7.startup = Main.startup
  , SB7.render = Main.render
  , SB7.shutdown = Main.shutdown
  , SB7.onKey = Main.onKey
  }
