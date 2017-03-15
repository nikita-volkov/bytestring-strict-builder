module Main where

import Prelude
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Data.ByteString as A
import qualified StrictBytesBuilder as B


main =
  defaultMain $
  testGroup "All tests" $
  [
    testProperty "Packing a list of bytes is isomorphic to appending a list of builders" $
    \byteList ->
      A.pack byteList ===
      B.run (foldMap B.byte byteList)
    ,
    testProperty "Concatting a list of bytestrings is isomorphic to fold-mapping with builders" $
    \bytesList ->
      mconcat bytesList ===
      B.run (foldMap B.bytes bytesList)
    ,
    testProperty "Concatting a list of bytestrings is isomorphic to concatting a list of builders" $
    \bytesList ->
      mconcat bytesList ===
      B.run (mconcat (map B.bytes bytesList))
  ]
