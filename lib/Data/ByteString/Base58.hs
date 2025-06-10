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

import Control.Monad (guard)
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
  guard (verify_base58 bs)
  let ls = leading_zeros bs
  pure $ ls <> unroll_base256 (roll_base58 bs)

verify_base58 :: BS.ByteString -> Bool
verify_base58 bs = case BS.uncons bs of
  Nothing -> True
  Just (h, t)
    | BS.elem h base58_charset -> verify_base58 t
    | otherwise -> False

base58_charset :: BS.ByteString
base58_charset = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

-- produce leading ones from leading zeros
leading_ones :: BS.ByteString -> BS.ByteString
leading_ones = go mempty where
  go acc bs = case BS.uncons bs of
    Nothing -> acc
    Just (h, t)
      | h == 0 -> go (BS.cons 0x31 acc) t
      | otherwise -> acc

-- produce leading zeros from leading ones
leading_zeros :: BS.ByteString -> BS.ByteString
leading_zeros = go mempty where
  go acc bs = case BS.uncons bs of
    Nothing -> acc
    Just (h, t)
      | h == 0x31 -> go (BS.cons 0x00 acc) t
      | otherwise -> acc

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

-- from base58
roll_base58 :: BS.ByteString -> Integer
roll_base58 bs = BS.foldl' alg 0 bs where
  alg !b !a = case word6 a of
    Just w -> b * 58 + fi w
    Nothing ->
      error "ppad-base58 (roll_base58): not a base58-encoded bytestring"

