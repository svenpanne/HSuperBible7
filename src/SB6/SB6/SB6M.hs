module SB6.SB6M (
  SB6M(..), parseSB6M,
  VertexData(..),
  VertexAttribData(..),
  IndexData(..),
  SubObject(..)
) where

import Control.Applicative( (<$>), (<*>) )
import qualified Control.Monad as M
import Data.Binary.Get ( Get, runGet, getByteString, bytesRead, skip,
                         getWord32le )
import Data.Bits ( (.|.), testBit, shift )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Foreign.Ptr ( Ptr, nullPtr, plusPtr )
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

data SB6M a = SB6M
  { vertexData :: VertexData
  , vertexAttribData :: [VertexAttribData a]
  , indexData :: Maybe IndexData
  , subObjectList :: [SubObject]
  , rawData :: BS.ByteString  -- ^ contains little-endian data
  } deriving ( Eq, Ord, Show )

parseSB6M :: BS.ByteString -> SB6M a
parseSB6M input = runGet (getSB6M input) $ BL.fromChunks [input]

getSB6M :: BS.ByteString -> Get (SB6M a)
getSB6M bs = do
  bodies <- getList getNumChunks getChunk
  let vd = one "vertex data" [ b | VertexDataChunk b <- bodies ]
      va = one "vertex attrib" [ b | VertexAttrib b <- bodies ]
      ix = optional "index data" [ b | IndexDataChunk b <- bodies ]
      sl = optional "object list" [ b | SubObjectList b <- bodies ]
      allVertices = SubObject { first = 0, count = totalVertices vd }
  return $ SB6M
    { vertexData = vd
    , vertexAttribData = va
    , indexData = ix
    , subObjectList = maybe [allVertices] id $ sl
    , rawData = bs }

one :: String -> [a] -> a
one what []  = error $ "missing " ++ what
one _    [x] = x
one what _   = error $ "duplicate " ++ what

optional :: String -> [a] -> Maybe a
optional _    [] = Nothing
optional what xs = Just $ one what xs

getNumChunks :: Get Int
getNumChunks = nc <$> getChunk
 where nc c = case c of
         FileHeaderChunk fh -> numChunks fh
         _ -> error "expected file header"

--------------------------------------------------------------------------------

getChunk :: Get (Chunk a)
getChunk = do
  x <- bytesRead
  h <- getChunkHeader
  val <- getChunkBody h
  y <- bytesRead
  skip (fromIntegral (fromIntegral (chunkSize h) - (y - x)))
  return val

--------------------------------------------------------------------------------

data ChunkHeader = ChunkHeader
  { chunkType :: ChunkType
  , chunkSize :: Int
  } deriving ( Eq, Ord, Show )

getChunkHeader :: Get ChunkHeader
getChunkHeader = ChunkHeader <$> getChunkType <*> getNum32le

chunkSizeWithoutHeader :: ChunkHeader -> Int
chunkSizeWithoutHeader = subtract 8 . chunkSize

--------------------------------------------------------------------------------

data ChunkType
  = ChunkTypeFileHeader
  | ChunkTypeIndexData
  | ChunkTypeVertexData
  | ChunkTypeVertexAttrib
  | ChunkTypeComment
  | ChunkTypeSubObjectList
  deriving ( Eq, Ord, Show )

getChunkType :: Get ChunkType
getChunkType = unmarshalChunkType <$> getWord32le
  where unmarshalChunkType x
          | x == fourCC 'S' 'B' '6' 'M' = ChunkTypeFileHeader
          | x == fourCC 'I' 'N' 'D' 'X' = ChunkTypeIndexData
          | x == fourCC 'V' 'R' 'T' 'X' = ChunkTypeVertexData
          | x == fourCC 'A' 'T' 'R' 'B' = ChunkTypeVertexAttrib
          | x == fourCC 'C' 'M' 'N' 'T' = ChunkTypeComment
          | x == fourCC 'O' 'L' 'S' 'T' = ChunkTypeSubObjectList
          | otherwise = error $ "unknwon chunk type " ++ show x
        fourCC a b c d = s a 0 .|. s b 8 .|. s c 16 .|. s d 24
        s x n = fromIntegral (fromEnum x) `shift` n

--------------------------------------------------------------------------------

data Chunk a
  = FileHeaderChunk FileHeader
  | IndexDataChunk IndexData
  | VertexDataChunk VertexData
  | VertexAttrib [VertexAttribData a]
  | Comment BS.ByteString
  | SubObjectList [SubObject]
  deriving ( Eq, Ord, Show )

getChunkBody :: ChunkHeader -> Get (Chunk a)
getChunkBody h = do
  let n = chunkSizeWithoutHeader h
  case chunkType h of
    ChunkTypeFileHeader -> FileHeaderChunk <$> getFileHeader
    ChunkTypeIndexData -> IndexDataChunk <$> getIndexData
    ChunkTypeVertexData -> VertexDataChunk <$> getVertexData
    ChunkTypeVertexAttrib ->
      VertexAttrib <$> getList getNum32le getVertexAttribData
    ChunkTypeComment -> Comment <$> getByteString n
    ChunkTypeSubObjectList -> SubObjectList <$> getList getNum32le getSubObject

--------------------------------------------------------------------------------

data FileHeader = FileHeader
  { numChunks :: Int
  , _flags :: Int
  } deriving ( Eq, Ord, Show )

getFileHeader :: Get FileHeader
getFileHeader = FileHeader <$> getNum32le <*> getNum32le

--------------------------------------------------------------------------------

data IndexData = IndexData
  { indexType :: DataType
  , indexCount :: GLsizei
  , indexDataOffset :: Int
  } deriving ( Eq, Ord, Show )

getIndexData :: Get IndexData
getIndexData = IndexData <$> getDataType <*> getNum32le <*> getNum32le

--------------------------------------------------------------------------------

data VertexData = VertexData
  { dataSize :: GLsizeiptr
  , dataOffset :: Int
  , totalVertices :: GLsizei
  } deriving ( Eq, Ord, Show )

getVertexData :: Get VertexData
getVertexData = VertexData <$> getNum32le <*> getNum32le <*> getNum32le

--------------------------------------------------------------------------------

data VertexAttribData a = VertexAttribData
  { attribName :: String
  , attribDescriptor :: VertexArrayDescriptor a
  , attribIntegerHandling :: IntegerHandling
  } deriving ( Eq, Ord, Show )

getVertexAttribData :: Get (VertexAttribData a)
getVertexAttribData = do
  n <- getCStringFixed 64
  s <- getNum32le
  t <- getDataType
  r <- getNum32le
  i <- getIntegerHandling
  o <- getBufferOffset
  return $ VertexAttribData
    { attribName = n
    , attribDescriptor = VertexArrayDescriptor s t r o
    , attribIntegerHandling = i }

getDataType :: Get DataType
getDataType = unmarshalDataType <$> getNum32le

getBufferOffset :: Get (Ptr a)
getBufferOffset = plusPtr nullPtr . fromIntegral <$> getWord32le

getIntegerHandling :: Get IntegerHandling
getIntegerHandling = v <$> getWord32le
  where v f | f `testBit` 1 = KeepIntegral
            | f `testBit` 0 = ToNormalizedFloat
            | otherwise     = ToFloat

--------------------------------------------------------------------------------

data SubObject = SubObject
  { first :: GLint
  , count :: GLsizei
  } deriving ( Eq, Ord, Show )

getSubObject :: Get SubObject
getSubObject = SubObject <$> getNum32le <*> getNum32le

--------------------------------------------------------------------------------
-- Various helpers

getList :: Get Int -> Get a -> Get [a]
getList getCount getElement = do
  c <- getCount
  M.replicateM c getElement

getNum32le :: Num a => Get a
getNum32le = fromIntegral <$> getWord32le

getCStringFixed :: Int -> Get String
getCStringFixed n = (unpackUtf8 . BS.takeWhile (/= 0)) <$> getByteString n

--------------------------------------------------------------------------------
-- TODO: Should we export this (and marshalDataType) from OpenGL?

unmarshalDataType :: GLenum -> DataType
unmarshalDataType x
   | x == gl_UNSIGNED_BYTE = UnsignedByte
   | x == gl_BYTE = Byte
   | x == gl_UNSIGNED_SHORT = UnsignedShort
   | x == gl_SHORT = Short
   | x == gl_UNSIGNED_INT = UnsignedInt
   | x == gl_INT = Int
   | x == gl_HALF_FLOAT = HalfFloat
   | x == gl_FLOAT = Float
   | x == gl_UNSIGNED_BYTE_3_3_2 = UnsignedByte332
   | x == gl_UNSIGNED_BYTE_2_3_3_REV = UnsignedByte233Rev
   | x == gl_UNSIGNED_SHORT_5_6_5 = UnsignedShort565
   | x == gl_UNSIGNED_SHORT_5_6_5_REV = UnsignedShort565Rev
   | x == gl_UNSIGNED_SHORT_4_4_4_4 = UnsignedShort4444
   | x == gl_UNSIGNED_SHORT_4_4_4_4_REV = UnsignedShort4444Rev
   | x == gl_UNSIGNED_SHORT_5_5_5_1 = UnsignedShort5551
   | x == gl_UNSIGNED_SHORT_1_5_5_5_REV = UnsignedShort1555Rev
   | x == gl_UNSIGNED_INT_8_8_8_8 = UnsignedInt8888
   | x == gl_UNSIGNED_INT_8_8_8_8_REV = UnsignedInt8888Rev
   | x == gl_UNSIGNED_INT_10_10_10_2 = UnsignedInt1010102
   | x == gl_UNSIGNED_INT_2_10_10_10_REV = UnsignedInt2101010Rev
   | x == gl_UNSIGNED_INT_24_8 = UnsignedInt248
   | x == gl_UNSIGNED_INT_10F_11F_11F_REV = UnsignedInt10f11f11fRev
   | x == gl_UNSIGNED_INT_5_9_9_9_REV = UnsignedInt5999Rev
   | x == gl_FLOAT_32_UNSIGNED_INT_24_8_REV = Float32UnsignedInt248Rev
   | x == gl_BITMAP = Bitmap
   | x == gl_UNSIGNED_SHORT_8_8_APPLE = UnsignedShort88
   | x == gl_UNSIGNED_SHORT_8_8_REV_APPLE = UnsignedShort88Rev
   | x == gl_DOUBLE = Double
   | x == gl_2_BYTES = TwoBytes
   | x == gl_3_BYTES = ThreeBytes
   | x == gl_4_BYTES = FourBytes
   | otherwise = error ("unmarshalDataType: illegal value " ++ show x)
