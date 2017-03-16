{-# LANGUAGE CPP #-}
module StrictBytesBuilder.Population where

import StrictBytesBuilder.Prelude
import qualified Data.ByteString.Internal as B
import qualified StrictBytesBuilder.UncheckedShifting as D


newtype Population =
  Population { populationPtrUpdate :: Ptr Word8 -> IO (Ptr Word8) }

instance Monoid Population where
  {-# INLINE mempty #-}
  mempty =
    Population return
  {-# INLINE mappend #-}
  mappend (Population leftPtrUpdate) (Population rightPtrUpdate) =
    Population (leftPtrUpdate >=> rightPtrUpdate)

instance Semigroup Population

-- |
-- Write the given bytes into the pointer and
-- return a pointer incremented by the amount of written bytes.
{-# INLINE bytes #-}
bytes :: B.ByteString -> Population
bytes (B.PS foreignPointer offset length) =
  Population $ \ptr ->
  withForeignPtr foreignPointer $ \ptr' ->
  B.memcpy ptr (plusPtr ptr' offset) length $> plusPtr ptr length

{-# INLINE byte #-}
byte :: Word8 -> Population
byte byte =
  Population $ \ptr ->
  poke ptr byte $> plusPtr ptr 1

{-# INLINE storable #-}
storable :: Storable a => Int -> a -> Population
storable size value =
  Population (\ptr -> poke (castPtr ptr) value $> plusPtr ptr size)

{-# INLINE word16BE #-}
word16BE :: Word16 -> Population
#ifdef WORDS_BIGENDIAN
word16BE =
  storable 2
#else
word16BE value =
  Population $ \ptr -> do
    poke ptr (fromIntegral (D.shiftr_w16 value 8) :: Word8)
    pokeByteOff ptr 1 (fromIntegral value :: Word8)
    return (plusPtr ptr 2)
#endif

{-# INLINE word32BE #-}
word32BE :: Word32 -> Population
#ifdef WORDS_BIGENDIAN
word32BE =
  storable 4
#else
word32BE value =
  Population $ \ptr -> do
    poke ptr (fromIntegral (D.shiftr_w32 value 24) :: Word8)
    pokeByteOff ptr 1 (fromIntegral (D.shiftr_w32 value 16) :: Word8)
    pokeByteOff ptr 2 (fromIntegral (D.shiftr_w32 value 8) :: Word8)
    pokeByteOff ptr 3 (fromIntegral value :: Word8)
    return (plusPtr ptr 4)
#endif

{-# INLINE word64BE #-}
word64BE :: Word64 -> Population
#ifdef WORDS_BIGENDIAN
word64BE =
  storable 8
#else
#if WORD_SIZE_IN_BITS < 64
--
-- To avoid expensive 64 bit shifts on 32 bit machines, we cast to
-- Word32, and write that
--
word64BE value =
  word32BE (fromIntegral (D.shiftr_w64 value 32)) <>
  word32BE (fromIntegral value)
#else
word64BE value =
  Population $ \ptr -> do
    poke ptr (fromIntegral (D.shiftr_w64 value 56) :: Word8)
    pokeByteOff ptr 1 (fromIntegral (D.shiftr_w64 value 48) :: Word8)
    pokeByteOff ptr 2 (fromIntegral (D.shiftr_w64 value 40) :: Word8)
    pokeByteOff ptr 3 (fromIntegral (D.shiftr_w64 value 32) :: Word8)
    pokeByteOff ptr 4 (fromIntegral (D.shiftr_w64 value 24) :: Word8)
    pokeByteOff ptr 5 (fromIntegral (D.shiftr_w64 value 16) :: Word8)
    pokeByteOff ptr 6 (fromIntegral (D.shiftr_w64 value  8) :: Word8)
    pokeByteOff ptr 7 (fromIntegral value :: Word8)
    return (plusPtr ptr 8)
#endif
#endif
