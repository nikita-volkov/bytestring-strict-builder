module Main where

import Prelude
import Criterion.Main
import ByteString.StrictBuilder


main =
  defaultMain [leftAppends, rightAppends]

leftAppends :: Benchmark
leftAppends =
  bench "leftAppends" $ whnf action $! replicate 1000 $ bytes "abc"
  where
    action bytesList =
      builderBytes builder
      where
        builder =
          foldl' (<>) mempty bytesList

rightAppends :: Benchmark
rightAppends =
  bench "rightAppends"  $ whnf action $! replicate 1000 $ bytes "abc"
  where
    action bytesList =
      builderBytes builder
      where
        builder =
          foldr (<>) mempty bytesList
