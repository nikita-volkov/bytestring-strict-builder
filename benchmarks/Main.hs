module Main where

import Prelude hiding (concat)
import Gauge.Main
import ByteString.StrictBuilder


main =
  defaultMain [leftAppends, rightAppends, concat]

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

concat :: Benchmark
concat =
  bench "concat"  $ whnf action $! replicate 10000 $ bytes "abc"
  where
    action bytesList =
      builderBytes (mconcat bytesList)
