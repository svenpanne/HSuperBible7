-- TODO: This should somehow be exported from OpenGL.
module SB6.DataType where

import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw

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
