{-# LANGUAGE CPP #-}
module StrictBytesBuilder
(
  Builder,
  builderBytes,
  builderLength,
  byte,
  bytes,
  asciiIntegral,
  asciiChar,
  storable,
  word16BE,
  word32BE,
  word64BE,
)
where

import StrictBytesBuilder.Prelude
import qualified StrictBytesBuilder.Population as A
import qualified StrictBytesBuilder.UncheckedShifting as D
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as C


data Builder =
  Builder !Int !A.Population

instance Monoid Builder where
  {-# INLINE mempty #-}
  mempty =
    Builder 0 mempty
  {-# INLINE mappend #-}
  mappend (Builder leftSize leftPopulation) (Builder rightSize rightPopulation) =
    Builder (leftSize + rightSize) (leftPopulation <> rightPopulation)

instance Semigroup Builder

instance IsString Builder where
  fromString =
    bytes . fromString


-- *
-------------------------

builderBytes :: Builder -> ByteString
builderBytes (Builder size population) =
  C.unsafeCreate size $ \ptr -> A.populationPtrUpdate population ptr $> ()

builderLength :: Builder -> Int
builderLength (Builder size population) =
  size


-- * Primitives
-------------------------

{-# INLINE bytes #-}
bytes :: ByteString -> Builder
bytes bytes =
  Builder (B.length bytes) (A.bytes bytes)

{-# INLINE byte #-}
byte :: Word8 -> Builder
byte byte =
  Builder 1 (A.byte byte)


-- * Extras
-------------------------

{-# INLINABLE asciiIntegral #-}
asciiIntegral :: Integral a => a -> Builder
asciiIntegral =
  \case
    0 ->
      byte 48
    x ->
      bool ((<>) (byte 45)) id (x >= 0) $
      loop mempty $
      abs x
  where
    loop builder remainder =
      case remainder of
        0 ->
          builder
        _ ->
          case quotRem remainder 10 of
            (quot, rem) ->
              loop (byte (48 + fromIntegral rem) <> builder) quot

{-# INLINE asciiChar #-}
asciiChar :: Char -> Builder
asciiChar =
  byte . fromIntegral . ord

{-# INLINE CONLIKE storable #-}
storable :: Storable a => a -> Builder
storable value =
  Builder size (A.storable size value)
  where
    size =
      sizeOf value

{-# INLINE word16BE #-}
word16BE :: Word16 -> Builder
word16BE =
  Builder 2 . A.word16BE

{-# INLINE word32BE #-}
word32BE :: Word32 -> Builder
word32BE =
  Builder 4 . A.word32BE

{-# INLINE word64BE #-}
word64BE :: Word64 -> Builder
word64BE =
  Builder 8 . A.word64BE
