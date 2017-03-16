{-# LANGUAGE CPP #-}
module StrictBytesBuilder
(
  Builder,
  builderBytes,
  builderLength,
  bytes,
  lazyBytes,
  asciiIntegral,
  asciiChar,
  utf8Char,
  storable,
  word8,
  word16BE,
  word32BE,
  word64BE,
  int8,
  int16BE,
  int32BE,
  int64BE,
)
where

import StrictBytesBuilder.Prelude
import qualified StrictBytesBuilder.Population as A
import qualified StrictBytesBuilder.UncheckedShifting as D
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as C
import qualified Data.ByteString.Lazy as F
import qualified StrictBytesBuilder.UTF8 as E


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

{-# INLINE lazyBytes #-}
lazyBytes :: F.ByteString -> Builder
lazyBytes =
  F.foldlChunks (\builder -> mappend builder . bytes) mempty

{-# INLINE byte #-}
byte :: Word8 -> Builder
byte =
  word8


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

{-# INLINE word8 #-}
word8 :: Word8 -> Builder
word8 =
  Builder 1 . A.word8

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

{-# INLINE int8 #-}
int8 :: Int8 -> Builder
int8 =
  Builder 1 . A.int8

{-# INLINE int16BE #-}
int16BE :: Int16 -> Builder
int16BE =
  Builder 2 . A.int16BE

{-# INLINE int32BE #-}
int32BE :: Int32 -> Builder
int32BE =
  Builder 4 . A.int32BE

{-# INLINE int64BE #-}
int64BE :: Int64 -> Builder
int64BE =
  Builder 8 . A.int64BE

{-# INLINE utf8Char #-}
utf8Char :: Char -> Builder
utf8Char x =
  E.char x
  (\a -> Builder 1 (A.bytes_1 a))
  (\a b -> Builder 2 (A.bytes_2 a b))
  (\a b c -> Builder 3 (A.bytes_3 a b c))
  (\a b c d -> Builder 4 (A.bytes_4 a b c d))
