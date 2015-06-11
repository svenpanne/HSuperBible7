module SB6.Pickle (
  -- * The Pickler\/Unpickler itself
  PU, puLift, puSeq,

  -- * Convenience runners
  runPickler, runUnpickler,

  -- * Combinators

  -- ** Tuples
  puPair, puTriple, pu4Tuple, pu5Tuple, pu6Tuple, pu7Tuple, pu8Tuple,

  -- ** Wrappers
  puWrap, puFromIntegral, puUnsafeCoerce,

  -- * Basic Picklers\/Unpicklers

  -- ** Primitive types
  puByteString, puLazyByteString,
  puStorable,
  puWord8,
  puInt8,

  -- ** Big-endian integrals
  puWord16be, puWord32be, puWord64be,
  puInt16be, puInt32be, puInt64be,
  puCFloatbe, puCDoublebe,

  -- ** Little-endian integrals
  puWord16le, puWord32le, puWord64le,
  puInt16le, puInt32le, puInt64le,
  puCFloatle, puCDoublele,

  -- ** Host-order integrals
  puWord16host, puWord32host, puWord64host,
  puInt16host, puInt32host, puInt64host,
  puCFloathost, puCDoublehost
) where

import Data.Binary.Builder.Internal ( writeN )
import Data.Binary.Get ( Get, runGet,
                         getByteString, getLazyByteString,
                         getWord8,
                         getWord16be, getWord16le, getWord16host,
                         getWord32be, getWord32le, getWord32host,
                         getWord64be, getWord64le, getWord64host )
import Data.Binary.Get.Internal ( readNWith )
import Data.Binary.Put ( Put, putBuilder, runPut,
                         putByteString, putLazyByteString,
                         putWord8,
                         putWord16be, putWord16le, putWord16host,
                         putWord32be, putWord32le, putWord32host,
                         putWord64be, putWord64le, putWord64host )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Int ( Int8, Int16, Int32, Int64 )
import Data.Word ( Word8, Word16, Word32, Word64 )
import Foreign.C.Types ( CFloat, CDouble )
import Foreign.Ptr ( castPtr )
import Foreign.Storable ( Storable(..) )
import Unsafe.Coerce ( unsafeCoerce )

--------------------------------------------------------------------------------

data PU a = PU {
  pickle :: a -> Put,
  unpickle :: Get a
  }

puLift :: a -> PU a
puLift x = PU {
  pickle = const $ return (),
  unpickle = return x
  }

puSeq :: (b -> a) -> PU a -> (a -> PU b) -> PU b
puSeq f pa k = PU {
  pickle = \b -> let a = f b in pickle pa a >> pickle (k a) b,
  unpickle = unpickle pa >>= (unpickle . k)
  }

--------------------------------------------------------------------------------

runPickler :: PU a -> a -> BL.ByteString
runPickler p = runPut . pickle p

runUnpickler :: PU a -> BL.ByteString -> a
runUnpickler p = runGet $ unpickle p

--------------------------------------------------------------------------------

puPair :: PU a -> PU b -> PU (a, b)
puPair pa pb =
  puSeq fst pa $ \a ->
  puSeq snd pb $ \b ->
  puLift (a, b)

puTriple :: PU a -> PU b -> PU c -> PU (a, b, c)
puTriple pa pb pc =
  puWrap
    (\(a, (b, c)) -> (a, b, c),
     \(a, b, c) -> (a, (b, c)))
    (puPair pa (puPair pb pc))

pu4Tuple :: PU a -> PU b -> PU c -> PU d -> PU (a, b, c, d)
pu4Tuple pa pb pc pd =
  puWrap
    (\((a, b), (c, d)) -> (a, b, c, d),
     \(a, b, c, d) -> ((a, b), (c, d)))
    (puPair (puPair pa pb) (puPair pc pd))

pu5Tuple :: PU a -> PU b -> PU c -> PU d -> PU e -> PU (a, b, c, d, e)
pu5Tuple pa pb pc pd pe =
  puWrap
    (\((a, b), (c, d, e)) -> (a, b, c, d, e),
     \(a, b, c, d, e) -> ((a, b), (c, d, e)))
    (puPair (puPair pa pb) (puTriple pc pd pe))

pu6Tuple :: PU a -> PU b -> PU c -> PU d -> PU e -> PU f -> PU (a, b, c, d, e, f)
pu6Tuple pa pb pc pd pe pf =
  puWrap
    (\((a, b, c), (d, e, f)) -> (a, b, c, d, e, f),
     \(a, b, c, d, e, f) -> ((a, b, c), (d, e, f)))
    (puPair (puTriple pa pb pc) (puTriple pd pe pf))

pu7Tuple :: PU a -> PU b -> PU c -> PU d -> PU e -> PU f -> PU g -> PU (a, b, c, d, e, f, g)
pu7Tuple pa pb pc pd pe pf pg =
  puWrap
    (\((a, b, c), (d, e, f, g)) -> (a, b, c, d, e, f, g),
     \(a, b, c, d, e, f, g) -> ((a, b, c), (d, e, f, g)))
    (puPair (puTriple pa pb pc) (pu4Tuple pd pe pf pg))

pu8Tuple :: PU a -> PU b -> PU c -> PU d -> PU e -> PU f -> PU g -> PU h -> PU (a, b, c, d, e, f, g, h)
pu8Tuple pa pb pc pd pe pf pg ph =
  puWrap
    (\((a, b, c, d), (e, f, g, h)) -> (a, b, c, d, e, f, g, h),
     \(a, b, c, d, e, f, g, h) -> ((a, b, c, d), (e, f, g, h)))
    (puPair (pu4Tuple pa pb pc pd) (pu4Tuple pe pf pg ph))

--------------------------------------------------------------------------------

puWrap :: (a -> b, b -> a) -> PU a -> PU b
puWrap (i, j) pa = puSeq j pa (puLift . i)

puFromIntegral :: (Integral a, Integral b) => PU a -> PU b
puFromIntegral = puWrap (fromIntegral, fromIntegral)

puUnsafeCoerce :: PU a -> PU b
puUnsafeCoerce = puWrap (unsafeCoerce, unsafeCoerce)

--------------------------------------------------------------------------------

puByteString :: Int -> PU BS.ByteString
puByteString len = PU {
  pickle = putByteString . BS.take len,
  unpickle = getByteString len
  }

puLazyByteString :: Int64 -> PU BL.ByteString
puLazyByteString len = PU {
  pickle = putLazyByteString . BL.take len,
  unpickle = getLazyByteString len
  }

--------------------------------------------------------------------------------

puStorable :: Storable a => PU a
puStorable = PU {
  pickle = \x -> putBuilder $ writeN (sizeOf x) (flip poke x . castPtr),
  unpickle = let helper :: Storable b => b -> Get b
                 helper dummy = readNWith (sizeOf dummy) peek
             in  helper undefined
  }

--------------------------------------------------------------------------------

puWord8 :: PU Word8
puWord8 = PU { pickle = putWord8, unpickle = getWord8 }

puInt8 :: PU Int8
puInt8 = puFromIntegral puWord8

--------------------------------------------------------------------------------

puWord16be :: PU Word16
puWord16be = PU { pickle = putWord16be, unpickle = getWord16be }

puWord32be :: PU Word32
puWord32be = PU { pickle = putWord32be, unpickle = getWord32be }

puWord64be :: PU Word64
puWord64be = PU { pickle = putWord64be, unpickle = getWord64be }

puInt16be :: PU Int16
puInt16be = puFromIntegral puWord16be

puInt32be :: PU Int32
puInt32be = puFromIntegral puWord32be

puInt64be :: PU Int64
puInt64be = puFromIntegral puWord64be

puCFloatbe :: PU CFloat
puCFloatbe = puUnsafeCoerce puWord32be

puCDoublebe :: PU CDouble
puCDoublebe = puUnsafeCoerce puWord64be

--------------------------------------------------------------------------------

puWord16le :: PU Word16
puWord16le = PU { pickle = putWord16le, unpickle = getWord16le }

puWord32le :: PU Word32
puWord32le = PU { pickle = putWord32le, unpickle = getWord32le }

puWord64le :: PU Word64
puWord64le = PU { pickle = putWord64le, unpickle = getWord64le }

puInt16le :: PU Int16
puInt16le = puFromIntegral puWord16le

puInt32le :: PU Int32
puInt32le = puFromIntegral puWord32le

puInt64le :: PU Int64
puInt64le = puFromIntegral puWord64le

puCFloatle :: PU CFloat
puCFloatle = puUnsafeCoerce puWord32le

puCDoublele :: PU CDouble
puCDoublele = puUnsafeCoerce puWord64le

--------------------------------------------------------------------------------

puWord16host :: PU Word16
puWord16host = PU { pickle = putWord16host, unpickle = getWord16host }

puWord32host :: PU Word32
puWord32host = PU { pickle = putWord32host, unpickle = getWord32host }

puWord64host :: PU Word64
puWord64host = PU { pickle = putWord64host, unpickle = getWord64host }

puInt16host :: PU Int16
puInt16host = puFromIntegral puWord16host

puInt32host :: PU Int32
puInt32host = puFromIntegral puWord32host

puInt64host :: PU Int64
puInt64host = puFromIntegral puWord64host

puCFloathost :: PU CFloat
puCFloathost = puUnsafeCoerce puWord32host

puCDoublehost :: PU CDouble
puCDoublehost = puUnsafeCoerce puWord64host
