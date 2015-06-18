module SB6.Object (
  Object, loadObject, freeObject, renderObject, renderSubObject
) where

import Control.Applicative( (<$>) )
import Control.Monad ( forM_ )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw
import SB6.SB6M

data Object = Object
  { vertexBuffer :: BufferObject
  , indexBuffer :: Maybe BufferObject
  , vao :: VertexArrayObject
  , numIndices :: GLsizei
  , indexDataType :: DataType
  , subObjects :: [SubObject]
  } deriving ( Eq, Ord, Show )

loadObject :: FilePath -> IO Object
loadObject filePath = do
  sb6m <- parseSB6M <$> BS.readFile filePath

  theVertexBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just theVertexBuffer
  BSU.unsafeUseAsCString (rawData sb6m) $ \ptr ->
    bufferData ArrayBuffer $=
      ( dataSize (vertexData sb6m)
      , ptr `plusPtr` dataOffset (vertexData sb6m)
      , StaticDraw )

  theVao <- genObjectName
  bindVertexArrayObject $= Just theVao

  forM_ (zip (map AttribLocation [0..]) (vertexAttribData sb6m)) $ \(loc, vad) -> do
    vertexAttribPointer loc $= (attribIntegerHandling vad, attribDescriptor vad)
    vertexAttribArray loc $= Enabled

  (ib, ni, it) <- maybe (return (Nothing, totalVertices (vertexData sb6m), undefined)) -- TODO ugly
        (\ix -> do theIndexBuffer <- genObjectName
                   bindBuffer ElementArrayBuffer $= Just theIndexBuffer
                   BSU.unsafeUseAsCString (rawData sb6m) $ \ptr -> do
                     bufferData ElementArrayBuffer $=
                       ( fromIntegral (indexCount ix) * fromIntegral (size (indexType ix))
                       , ptr `plusPtr` indexDataOffset ix
                       , StaticDraw )
                   return (Just theIndexBuffer, indexCount ix, indexType ix))
        (indexData sb6m)

  bindVertexArrayObject $= Nothing
  bindBuffer ElementArrayBuffer $= Nothing

  return $ Object
    { vertexBuffer = theVertexBuffer
    , indexBuffer = ib
    , vao = theVao
    , numIndices = ni
    , indexDataType = it
    , subObjects = subObjectList sb6m }

size :: DataType -> Int
size x = case x of
  UnsignedByte -> sizeOf (undefined :: GLubyte)
  Byte -> sizeOf (undefined :: GLbyte)
  UnsignedShort -> sizeOf (undefined :: GLushort)
  Short -> sizeOf (undefined :: GLshort)
  UnsignedInt -> sizeOf (undefined :: GLuint)
  Int -> sizeOf (undefined :: GLint)
  HalfFloat -> sizeOf (undefined :: GLhalf)
  Float -> sizeOf (undefined :: GLfloat)
  UnsignedByte332 -> sizeOf (undefined :: GLubyte)
  UnsignedByte233Rev -> sizeOf (undefined :: GLubyte)
  UnsignedShort565 ->  sizeOf (undefined :: GLushort)
  UnsignedShort565Rev -> sizeOf (undefined :: GLushort)
  UnsignedShort4444 -> sizeOf (undefined :: GLushort)
  UnsignedShort4444Rev -> sizeOf (undefined :: GLushort)
  UnsignedShort5551 -> sizeOf (undefined :: GLushort)
  UnsignedShort1555Rev -> sizeOf (undefined :: GLushort)
  UnsignedInt8888 -> sizeOf (undefined :: GLint)
  UnsignedInt8888Rev -> sizeOf (undefined :: GLint)
  UnsignedInt1010102 -> sizeOf (undefined :: GLint)
  UnsignedInt2101010Rev -> sizeOf (undefined :: GLint)
  UnsignedInt248 -> sizeOf (undefined :: GLint)
  UnsignedInt10f11f11fRev -> sizeOf (undefined :: GLint)
  UnsignedInt5999Rev -> sizeOf (undefined :: GLint)
  Float32UnsignedInt248Rev -> 8
  Bitmap -> 0
  UnsignedShort88 -> sizeOf (undefined :: GLushort)
  UnsignedShort88Rev -> sizeOf (undefined :: GLushort)
  Double -> sizeOf (undefined :: GLdouble)
  TwoBytes -> 2
  ThreeBytes -> 3
  FourBytes -> 3

freeObject :: Object -> IO ()
freeObject object = do
  deleteObjectName $ vao object
  deleteObjectName $ vertexBuffer object
  maybe (return ()) deleteObjectName (indexBuffer object)

renderObject :: Object -> IO ()
renderObject object = renderSubObject object 0 1 0

renderSubObject :: Object -> Int -> GLsizei -> GLuint -> IO ()
renderSubObject object objectIndex instanceCount baseInstance = do
  bindVertexArrayObject $= Just (vao object)
  maybe (let subObj = subObjects object !! objectIndex -- TODO Use array
         in glDrawArraysInstancedBaseInstance gl_TRIANGLES
                                              (first subObj)
                                              (count subObj)
                                              instanceCount
                                              baseInstance)
        (\_ -> glDrawElementsInstancedBaseInstance gl_TRIANGLES
                                                   (numIndices object)
                                                   (undefined (indexDataType object)) -- !!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                   nullPtr
                                                   instanceCount
                                                   baseInstance)
       (indexBuffer object)
