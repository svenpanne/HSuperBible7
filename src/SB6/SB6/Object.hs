module SB6.Object (
  Object, loadObject, freeObject, renderObject, renderSubObject
) where

import Control.Applicative( (<$>) )
import Control.Monad ( forM_ )
import Data.Array ( Array, (!) )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Foreign.Ptr  ( Ptr, nullPtr, plusPtr )
import Foreign.Storable ( sizeOf )
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw
import SB6.SB6M

data Object = Object
  { vertexBuffer :: BufferObject
  , indexBufferTypeNum :: Maybe (BufferObject, DataType, GLsizei)
  , vao :: VertexArrayObject
  , subObjects :: Array Int SubObject
  } deriving ( Eq, Ord, Show )

loadObject :: FilePath -> IO Object
loadObject filePath = do
  sb6m <- parseSB6M <$> BS.readFile filePath

  theVertexBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just theVertexBuffer
  withRawDataAtOffset sb6m (dataOffset (vertexData sb6m)) $ \ptr ->
    bufferData ArrayBuffer $= ( dataSize (vertexData sb6m), ptr, StaticDraw )

  theVao <- genObjectName
  bindVertexArrayObject $= Just theVao

  forM_ (zip (map AttribLocation [0..])
             (vertexAttribData sb6m)) $ \(loc, vad) -> do
    vertexAttribPointer loc $= (attribIntegerHandling vad, attribDescriptor vad)
    vertexAttribArray loc $= Enabled

  theIndexBufferTypeNum <- maybe
    (return Nothing)
    (\ix -> do theIndexBuffer <- genObjectName
               bindBuffer ElementArrayBuffer $= Just theIndexBuffer
               withRawDataAtOffset sb6m (indexDataOffset ix) $ \ptr -> do
                 bufferData ElementArrayBuffer $= ( size ix , ptr , StaticDraw )
               return $ Just (theIndexBuffer, indexType ix, indexCount ix))
    (indexData sb6m)

  bindVertexArrayObject $= Nothing
  bindBuffer ElementArrayBuffer $= Nothing

  return $ Object
    { vertexBuffer = theVertexBuffer
    , indexBufferTypeNum = theIndexBufferTypeNum
    , vao = theVao
    , subObjects = subObjectList sb6m }

withRawDataAtOffset :: SB6M a -> Int -> (Ptr b -> IO c) -> IO c
withRawDataAtOffset sb6m offset f =
  BSU.unsafeUseAsCString (rawData sb6m) $ \ptr ->
    f (ptr `plusPtr` offset)

freeObject :: Object -> IO ()
freeObject object = do
  deleteObjectName $ vao object
  deleteObjectName $ vertexBuffer object
  maybe (return ()) (\(b,_,_) -> deleteObjectName b) $ indexBufferTypeNum object

renderObject :: Object -> IO ()
renderObject object = renderSubObject object 0 1 0

renderSubObject :: Object -> Int -> GLsizei -> GLuint -> IO ()
renderSubObject object objectIndex instanceCount baseInstance = do
  bindVertexArrayObject $= Just (vao object)
  maybe (let subObj = subObjects object ! objectIndex
         in glDrawArraysInstancedBaseInstance gl_TRIANGLES
                                              (first subObj)
                                              (count subObj)
                                              instanceCount
                                              baseInstance)
        (\(_,t,n) -> glDrawElementsInstancedBaseInstance gl_TRIANGLES
                                                         n
                                                         (marshalDataType t)
                                                         nullPtr
                                                         instanceCount
                                                         baseInstance)
       (indexBufferTypeNum object)

-- TODO: Should we export this (and unmarshalDataType) from OpenGL?
marshalDataType :: DataType -> GLenum
marshalDataType x = case x of
   UnsignedByte -> gl_UNSIGNED_BYTE
   Byte -> gl_BYTE
   UnsignedShort -> gl_UNSIGNED_SHORT
   Short -> gl_SHORT
   UnsignedInt -> gl_UNSIGNED_INT
   Int -> gl_INT
   HalfFloat -> gl_HALF_FLOAT
   Float -> gl_FLOAT
   UnsignedByte332 -> gl_UNSIGNED_BYTE_3_3_2
   UnsignedByte233Rev -> gl_UNSIGNED_BYTE_2_3_3_REV
   UnsignedShort565 -> gl_UNSIGNED_SHORT_5_6_5
   UnsignedShort565Rev -> gl_UNSIGNED_SHORT_5_6_5_REV
   UnsignedShort4444 -> gl_UNSIGNED_SHORT_4_4_4_4
   UnsignedShort4444Rev -> gl_UNSIGNED_SHORT_4_4_4_4_REV
   UnsignedShort5551 -> gl_UNSIGNED_SHORT_5_5_5_1
   UnsignedShort1555Rev -> gl_UNSIGNED_SHORT_1_5_5_5_REV
   UnsignedInt8888 -> gl_UNSIGNED_INT_8_8_8_8
   UnsignedInt8888Rev -> gl_UNSIGNED_INT_8_8_8_8_REV
   UnsignedInt1010102 -> gl_UNSIGNED_INT_10_10_10_2
   UnsignedInt2101010Rev -> gl_UNSIGNED_INT_2_10_10_10_REV
   UnsignedInt248 -> gl_UNSIGNED_INT_24_8
   UnsignedInt10f11f11fRev -> gl_UNSIGNED_INT_10F_11F_11F_REV
   UnsignedInt5999Rev -> gl_UNSIGNED_INT_5_9_9_9_REV
   Float32UnsignedInt248Rev -> gl_FLOAT_32_UNSIGNED_INT_24_8_REV
   Bitmap -> gl_BITMAP
   UnsignedShort88 -> gl_UNSIGNED_SHORT_8_8_APPLE
   UnsignedShort88Rev -> gl_UNSIGNED_SHORT_8_8_REV_APPLE
   Double -> gl_DOUBLE
   TwoBytes -> gl_2_BYTES
   ThreeBytes -> gl_3_BYTES
   FourBytes -> gl_4_BYTES

size :: IndexData -> GLsizeiptr
size ix =
  fromIntegral (indexCount ix) * fromIntegral (dataTypeSize (indexType ix))

dataTypeSize :: DataType -> Int
dataTypeSize x = case x of
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
