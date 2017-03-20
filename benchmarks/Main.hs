module Main where

import Prelude
import Criterion.Main
import ByteString.StrictBuilder


main =
  defaultMain [leftAppends, rightAppends]

leftAppends :: Benchmark
leftAppends =
  bench "leftAppends" $ whnf action $ ()
  where
    action _ =
      builderBytes builder
      where
        builder =
          ((bytes "abc" <> bytes "abc") <> bytes "abc") <> bytes "abc"

rightAppends :: Benchmark
rightAppends =
  bench "rightAppends" $ whnf action $ ()
  where
    action _ =
      builderBytes builder
      where
        builder =
          bytes "abc" <> (bytes "abc" <> (bytes "abc" <> bytes "abc"))
