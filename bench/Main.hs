{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base58 as B58
import qualified Data.ByteString.Base58Check as B58C

main :: IO ()
main = defaultMain [
    base32
  ]

base32 :: Benchmark
base32 = bgroup "ppad-base32" [
    bgroup "base58" [
      bgroup "encode" [
        bench "hello world" $ nf B58.encode "hello world"
      ]
    , bgroup "decode" [
        bench "StV1DL6CwTryKyV" $ nf B58.decode "StV1DL6CwTryKyV"
      ]
    ]
  , bgroup "base58check" [
      bgroup "encode" [
        bench "0x00, hello world" $ nf (B58C.encode 0x00) "hello world"
      ]
    , bgroup "decode" [
        bench "13vQB7B6MrGQZaxCqW9KER" $
          nf B58C.decode "13vQB7B6MrGQZaxCqW9KER"
      ]
    ]
  ]