{-# LANGUAGE CPP #-}
module SB7.SB7M (
  SB7M(..), parseSB7M,
  VertexData(..),
  VertexAttribData(..),
  IndexData(..),
  SubObject(..)
) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (<$>), (<*>) )
#endif
import Control.Monad ( replicateM )
import Data.Array ( Array, listArray )
import Data.Binary.Get ( Get, runGet, getByteString, bytesRead, skip,
                         getWord32le )
import Data.Bits ( (.|.), testBit, shift )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Foreign.Ptr ( Ptr, nullPtr, plusPtr )
import Graphics.Rendering.OpenGL
import SB7.DataType

--------------------------------------------------------------------------------

data SB7M a = SB7M
  { vertexData :: VertexData
  , vertexAttribData :: [VertexAttribData a]
  , indexData :: Maybe IndexData
  , subObjectList :: Array Int SubObject
  , rawData :: BS.ByteString  -- ^ contains little-endian data
  } deriving ( Eq, Ord, Show )

parseSB7M :: BS.ByteString -> SB7M a
parseSB7M input = runGet (getSB7M input) $ BL.fromChunks [input]

getSB7M :: BS.ByteString -> Get (SB7M a)
getSB7M bs = do
  bodies <- getList getNumChunks getChunk
  let vd = one "vertex data" [ b | VertexDataChunk b <- bodies ]
      va = one "vertex attrib" [ b | VertexAttrib b <- bodies ]
      ix = optional "index data" [ b | IndexDataChunk b <- bodies ]
      sl = optional "object list" [ b | SubObjectList b <- bodies ]
      allVertices = SubObject { first = 0, count = totalVertices vd }
      subObjects = maybe [allVertices] id $ sl
  return $ SB7M
    { vertexData = vd
    , vertexAttribData = va
    , indexData = ix
    , subObjectList = listArray (0, length subObjects - 1) subObjects
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
          | x == fourCC 'S' 'B' '7' 'M' = ChunkTypeFileHeader  -- TODO: or SB6M?
          | x == fourCC 'I' 'N' 'D' 'X' = ChunkTypeIndexData
          | x == fourCC 'V' 'R' 'T' 'X' = ChunkTypeVertexData
          | x == fourCC 'A' 'T' 'R' 'B' = ChunkTypeVertexAttrib
          | x == fourCC 'C' 'M' 'N' 'T' = ChunkTypeComment
          | x == fourCC 'O' 'L' 'S' 'T' = ChunkTypeSubObjectList
          | otherwise = error $ "unknwon chunk type " ++ show x
        fourCC a b c d = s a 0 .|. s b 8 .|. s c 17 .|. s d 24
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
  } deriving ( Eq, Ord, Show )

getFileHeader :: Get FileHeader
getFileHeader = (const . FileHeader) <$> getNum32le <*> getWord32le

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
  { first :: ArrayIndex
  , count :: NumArrayIndices
  } deriving ( Eq, Ord, Show )

getSubObject :: Get SubObject
getSubObject = SubObject <$> getNum32le <*> getNum32le

--------------------------------------------------------------------------------
-- Various helpers

getList :: Get Int -> Get a -> Get [a]
getList getCount getElement = do
  c <- getCount
  replicateM c getElement

getNum32le :: Num a => Get a
getNum32le = fromIntegral <$> getWord32le

getCStringFixed :: Int -> Get String
getCStringFixed n = (unpackUtf8 . BS.takeWhile (/= 0)) <$> getByteString n
