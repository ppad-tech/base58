{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Aeson ((.:))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base58 as B58
import qualified Data.ByteString.Base58Check as B58Check
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as Q

data Valid_Base58Check = Valid_Base58Check {
    vc_string  :: !BS.ByteString
  , vc_payload :: !BS.ByteString
  } deriving Show

hex :: BS.ByteString -> AT.Parser BS.ByteString
hex bs = case B16.decode bs of
  Nothing -> fail "invalid hex"
  Just v  -> pure v

instance A.FromJSON Valid_Base58Check where
  parseJSON = A.withObject "Valid_Base58Check" $ \m -> Valid_Base58Check
    <$> fmap TE.encodeUtf8 (m .: "string")
    <*> (hex . TE.encodeUtf8 =<< (m .: "payload"))

data Invalid_Base58Check = Invalid_Base58Check {
    ic_string  :: !BS.ByteString
  } deriving Show

instance A.FromJSON Invalid_Base58Check where
  parseJSON = A.withObject "Invalid_Base58Check" $ \m -> Invalid_Base58Check
    <$> fmap TE.encodeUtf8 (m .: "string")

data Base58Check = Base58Check {
    b58c_valid   :: ![Valid_Base58Check]
  , b58c_invalid :: ![Invalid_Base58Check]
  } deriving Show

instance A.FromJSON Base58Check where
  parseJSON = A.withObject "Base58Check" $ \m -> Base58Check
    <$> (m .: "valid")
    <*> (m .: "invalid")


execute_base58check :: Base58Check -> TestTree
execute_base58check Base58Check {..} = testGroup "base58check" [
      testGroup "valid" (fmap execute_valid b58c_valid)
    , testGroup "invalid" (fmap execute_invalid b58c_invalid)
    ]
  where
    execute_valid Valid_Base58Check {..} = testGroup (label vc_string) [
        testCase "encode" $
          assertEqual mempty vc_string (B58Check.encode vc_payload)
      , testCase "decode" $
          assertEqual mempty (Just vc_payload) (B58Check.decode vc_string)
      ]

    execute_invalid Invalid_Base58Check {..} =
      testCase (label ic_string) $
        assertEqual mempty Nothing (B58Check.decode ic_string)

data Valid_Base58 = Valid_Base58 {
    vb_decodedHex  :: !BS.ByteString
  , vb_encoded     :: !BS.ByteString
  } deriving Show

instance A.FromJSON Valid_Base58 where
  parseJSON = A.withObject "Valid_Base58" $ \m -> Valid_Base58
    <$> (hex . TE.encodeUtf8 =<< (m .: "decodedHex"))
    <*> fmap TE.encodeUtf8 (m .: "encoded")

execute_base58 :: Valid_Base58 -> TestTree
execute_base58 Valid_Base58 {..} = testGroup (label vb_encoded) [
    testCase "encode" $
      assertEqual mempty vb_encoded (B58.encode vb_decodedHex)
  , testCase "decode" $
      assertEqual mempty (Just vb_decodedHex) (B58.decode vb_encoded)
  ]

-- test label for an encoded string
label :: BS.ByteString -> String
label bs
  | BS.null bs = "(empty)"
  | otherwise  = show (B8.unpack bs)

-- strings containing non-base58 characters
invalid_base58 :: [BS.ByteString]
invalid_base58 = [
    "0"
  , "O"
  , "I"
  , "l"
  , " "
  , "\NUL"
  , "+"
  , "/"
  , "\xff"
  , "\xc3\xa9"
  , "StV1DL0CwTryKyV"
  , "StV1DLOCwTryKyV"
  , "StV1DLICwTryKyV"
  , "StV1DLlCwTryKyV"
  , " StV1DL6CwTryKyV"
  , "StV1DL6CwTryKyV "
  , "StV1DL6CwTryKyV\n"
  , "11110"
  , "111l"
  , "11 1"
  ]

execute_invalid_base58 :: BS.ByteString -> TestTree
execute_invalid_base58 bs = testCase (label bs) $
  assertEqual mempty Nothing (B58.decode bs)

-- strings that base58-decode to fewer than four bytes
short_base58check :: [BS.ByteString]
short_base58check = ["", "1", "11", "111", "2g", "a3gV"]

execute_short_base58check :: BS.ByteString -> TestTree
execute_short_base58check bs = testCase (label bs) $
  assertEqual mempty Nothing (B58Check.decode bs)

newtype BS = BS BS.ByteString
  deriving (Eq, Show)

bytes :: Int -> Q.Gen BS.ByteString
bytes k = do
  l <- Q.chooseInt (0, k)
  v <- Q.vectorOf l Q.arbitrary
  pure (BS.pack v)

instance Q.Arbitrary BS where
  arbitrary = do
    b <- bytes 1024
    pure (BS b)

-- arbitrary strings over the base58 alphabet, plus some invalid
-- characters
newtype B58Str = B58Str BS.ByteString
  deriving (Eq, Show)

instance Q.Arbitrary B58Str where
  arbitrary = do
    l <- Q.chooseInt (0, 64)
    v <- Q.vectorOf l $ Q.frequency [
        (16, Q.elements alphabet)
      , (1, Q.elements (BS.unpack "0OIl +/\NUL\xff"))
      ]
    pure (B58Str (BS.pack v))
    where
      alphabet = BS.unpack
        "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

-- decoding succeeds exactly on strings over the alphabet, and every
-- such string is canonical
base58_decode_canonical :: B58Str -> Bool
base58_decode_canonical (B58Str bs) = case B58.decode bs of
  Nothing -> BS.any (`BS.notElem` alphabet) bs
  Just d  -> B58.encode d == bs
  where
    alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

base58_decode_inverts_encode :: BS -> Bool
base58_decode_inverts_encode (BS bs) = case B58.decode (B58.encode bs) of
  Nothing -> False
  Just b  -> b == bs

base58check_decode_inverts_encode :: BS -> Bool
base58check_decode_inverts_encode (BS bs) =
  case B58Check.decode (B58Check.encode bs) of
    Nothing -> False
    Just b  -> b == bs

main :: IO ()
main = do
  scure_base58 <- TIO.readFile "etc/base58.json"
  scure_base58check <- TIO.readFile "etc/base58_check.json"
  let per = do
        b0 <- A.decodeStrictText scure_base58 :: Maybe [Valid_Base58]
        b1 <- A.decodeStrictText scure_base58check :: Maybe Base58Check
        pure (b0, b1)
  case per of
    Nothing -> error "couldn't parse vectors"
    Just (b58, b58c) -> defaultMain $ testGroup "ppad-base58" [
        testGroup "unit tests" [
            testGroup "base58" (fmap execute_base58 b58)
          , testGroup "base58 (invalid)"
              (fmap execute_invalid_base58 invalid_base58)
          , execute_base58check b58c
          , testGroup "base58check (short)"
              (fmap execute_short_base58check short_base58check)
          ]
      , testGroup "property tests" [
          Q.testProperty "(base58) decode . encode ~ id" $
            Q.withMaxSuccess 250 base58_decode_inverts_encode
        , Q.testProperty "(base58) decode succeeds iff canonical" $
            Q.withMaxSuccess 1000 base58_decode_canonical
        , Q.testProperty "(base58check) decode . encode ~ id" $
            Q.withMaxSuccess 250 base58check_decode_inverts_encode
        ]
      ]

