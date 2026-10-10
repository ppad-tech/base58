{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Data.ByteString.Base58
-- Copyright: (c) 2024 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- base58 encoding and decoding of strict bytestrings.

module Data.ByteString.Base58 (
    encode
  , decode
  ) where

import qualified Data.Bits as B
import Data.Bits ((.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BU
import Data.Word (Word8)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

-- word8 base58 character to word6 (ish)
word6 :: Word8 -> Maybe Word8
word6 c
  | c >= 49  && c <= 57  = pure $! c - 49 -- 1–9
  | c >= 65  && c <= 72  = pure $! c - 56 -- A–H
  | c >= 74  && c <= 78  = pure $! c - 57 -- J–N
  | c >= 80  && c <= 90  = pure $! c - 58 -- P–Z
  | c >= 97  && c <= 107 = pure $! c - 64 -- a–k
  | c >= 109 && c <= 122 = pure $! c - 65 -- m–z
  | otherwise = Nothing
{-# INLINE word6 #-}

-- | Encode a base256 'ByteString' as base58.
--
--   >>> encode "hello world"
--   "StV1DL6CwTryKyV"
encode :: BS.ByteString -> BS.ByteString
encode bs = ls <> unroll_base58 (roll_base256 bs) where
  ls = leading_ones bs

-- | Decode a base58 'ByteString' to base256.
--
--   Invalid inputs will produce 'Nothing'.
--
--   >>> decode "StV1DL6CwTryKyV"
--   Just "hello world"
--   >>> decode "StV1DL0CwTryKyV" -- s/6/0
--   Nothing
decode :: BS.ByteString -> Maybe BS.ByteString
decode bs = do
  n <- roll_base58 bs
  let ls = leading_zeros bs
  pure $ ls <> unroll_base256 n

base58_charset :: BS.ByteString
base58_charset = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

-- produce leading ones from leading zeros
leading_ones :: BS.ByteString -> BS.ByteString
leading_ones bs = BS.replicate (BS.length (BS.takeWhile (== 0x00) bs)) 0x31

-- produce leading zeros from leading ones
leading_zeros :: BS.ByteString -> BS.ByteString
leading_zeros bs = BS.replicate (BS.length (BS.takeWhile (== 0x31) bs)) 0x00

-- to base256
unroll_base256 :: Integer -> BS.ByteString
unroll_base256 = BS.reverse . BS.unfoldr coalg where
  coalg a
    | a == 0 = Nothing
    | otherwise = Just $
        let (b, c) = quotRem a 256
        in  (fi c, b)

-- from base256
roll_base256 :: BS.ByteString -> Integer
roll_base256 = BS.foldl' alg 0 where
  alg !a !b = a `B.shiftL` 8 .|. fi b

-- to base58
unroll_base58 :: Integer -> BS.ByteString
unroll_base58 = BS.reverse . BS.unfoldr coalg where
  coalg a
    | a == 0 = Nothing
    | otherwise = Just $
        let (b, c) = quotRem a 58
        in  (BU.unsafeIndex base58_charset (fi c), b)

-- from base58, failing on any non-base58 character
roll_base58 :: BS.ByteString -> Maybe Integer
roll_base58 bs = go 0 0 where
  l = BS.length bs
  go !acc !i
    | i == l = Just acc
    | otherwise = case word6 (BU.unsafeIndex bs i) of
        Nothing -> Nothing
        Just w  -> go (acc * 58 + fi w) (i + 1)

