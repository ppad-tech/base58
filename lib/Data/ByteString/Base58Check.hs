{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Data.ByteString.Base58Check
-- Copyright: (c) 2024 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- base58check encoding and decoding of strict bytestrings.
--
-- base58check is a versioned, checksummed base58 encoding. A checksum
-- is computed by SHA256d-ing a payload, appending its first 4 bytes,
-- and base58-encoding the result.

module Data.ByteString.Base58Check (
    encode
  , decode
  ) where

import Control.Monad (guard)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base58 as B58

-- | Encode a base256 'ByteString' as base58check.
--
--   >>> encode (BS.singleton 0x00 <> "hello world")
--   "13vQB7B6MrGQZaxCqW9KER"
encode :: BS.ByteString -> BS.ByteString
encode pay =
  let kek = BS.take 4 (SHA256.hash (SHA256.hash pay))
  in  B58.encode (pay <> kek)

-- | Validate and decode a base58check-encoded string. Invalid
--   base58check inputs will produce 'Nothing'.
--
--   >>> decode "13vQB7B6MrGQZaxCqW9KER"
--   Just "\NULhello world"
--   >>> decode "13uQB7B6MrGQZaxCqW9KER" -- s/v/u
--   Nothing
decode :: BS.ByteString -> Maybe BS.ByteString
decode mb = do
  bs <- B58.decode mb
  let len = BS.length bs
      (pay, kek) = BS.splitAt (len - 4) bs
      man = BS.take 4 (SHA256.hash (SHA256.hash pay))
  guard (kek == man)
  pure pay

