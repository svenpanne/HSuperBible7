module SB6.SB6M (
  SB6M(..), getSB6M,
  VertexData(..),
  VertexAttribData(..), VertexAttribFlags(..),
  IndexData(..),
  SubObject(..)
) where

import Control.Applicative( (<$>), (<*>) )
import qualified Control.Monad as M
import Data.Binary.Get ( Get, getByteString, bytesRead, skip, getWord32le )
import Data.Bits ( (.|.), testBit, shift )
import qualified Data.ByteString as BS
import qualified Graphics.Rendering.OpenGL as GL

--------------------------------------------------------------------------------

data SB6M = SB6M
  { vertexData :: VertexData
  , vertexAttribData :: [VertexAttribData]
  , indexData :: Maybe IndexData
  , subObjectList :: [SubObject]
  , rawData :: BS.ByteString  -- ^ contains little-endian data
  } deriving ( Eq, Ord, Show )

getSB6M :: BS.ByteString -> Get SB6M
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

getChunk :: Get Chunk
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

data Chunk
  = FileHeaderChunk FileHeader
  | IndexDataChunk IndexData
  | VertexDataChunk VertexData
  | VertexAttrib [VertexAttribData]
  | Comment BS.ByteString
  | SubObjectList [SubObject]
  deriving ( Eq, Ord, Show )

getChunkBody :: ChunkHeader -> Get Chunk
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
  { indexType :: GL.GLenum
  , indexCount :: GL.GLsizei
  , indexDataOffset :: Int
  } deriving ( Eq, Ord, Show )

getIndexData :: Get IndexData
getIndexData = IndexData <$> getNum32le <*> getNum32le <*> getNum32le

--------------------------------------------------------------------------------

data VertexData = VertexData
  { dataSize :: GL.GLsizeiptr
  , dataOffset :: Int
  , totalVertices :: GL.GLsizei
  } deriving ( Eq, Ord, Show )

getVertexData :: Get VertexData
getVertexData = VertexData <$> getNum32le <*> getNum32le <*> getNum32le

--------------------------------------------------------------------------------

data VertexAttribData = VertexAttribData
  { attribName :: String
  , attribSize :: GL.GLint
  , attribType :: GL.GLenum
  , attribStride :: GL.GLsizei
  , attribFlags :: VertexAttribFlags
  , attribDataOffset :: Int
  } deriving ( Eq, Ord, Show )

getVertexAttribData :: Get VertexAttribData
getVertexAttribData =
  VertexAttribData <$> getCStringFixed 64 <*> getNum32le <*> getNum32le <*>
                       getNum32le <*> getVertexAttribFlags <*> getNum32le

--------------------------------------------------------------------------------

data VertexAttribFlags = VertexAttribFlags
  { isNormalized :: Bool
  , isInteger :: Bool
  } deriving ( Eq, Ord, Show )

getVertexAttribFlags :: Get VertexAttribFlags
getVertexAttribFlags = v <$> getWord32le
  where v f = VertexAttribFlags (f `testBit` 0) (f `testBit` 1)

--------------------------------------------------------------------------------

data SubObject = SubObject
  { first :: GL.GLint
  , count :: GL.GLsizei
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
getCStringFixed n = (GL.unpackUtf8 . BS.takeWhile (/= 0)) <$> getByteString n
