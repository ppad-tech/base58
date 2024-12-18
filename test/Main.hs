{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Aeson ((.:))
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base58 as B58
import qualified Data.ByteString.Base58Check as B58Check
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Test.Tasty
import Test.Tasty.HUnit

data Valid_Base58Check = Valid_Base58Check {
    vc_string  :: !BS.ByteString
  , vc_payload :: !BS.ByteString
  } deriving Show

instance A.FromJSON Valid_Base58Check where
  parseJSON = A.withObject "Valid_Base58Check" $ \m -> Valid_Base58Check
    <$> fmap TE.encodeUtf8 (m .: "string")
    <*> fmap (B16.decodeLenient . TE.encodeUtf8) (m .: "payload")

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
    execute_valid Valid_Base58Check {..} = testCase "valid" $ do -- label
      let enc = case BS.uncons vc_payload of
            Nothing -> error "faulty"
            Just (h, t) -> B58Check.encode h t
      assertEqual mempty enc vc_string

    execute_invalid Invalid_Base58Check {..} = testCase "invalid" $ do -- label
      let dec = B58Check.decode ic_string
          is_just = \case
            Nothing -> False
            Just _ -> True
      assertBool mempty (not (is_just dec))

data Valid_Base58 = Valid_Base58 {
    vb_decodedHex  :: !BS.ByteString
  , vb_encoded     :: !BS.ByteString
  } deriving Show

instance A.FromJSON Valid_Base58 where
  parseJSON = A.withObject "Valid_Base58" $ \m -> Valid_Base58
    <$> fmap (B16.decodeLenient . TE.encodeUtf8) (m .: "decodedHex")
    <*> fmap TE.encodeUtf8 (m .: "encoded")

execute_base58 :: Valid_Base58 -> TestTree -- XX label
execute_base58 Valid_Base58 {..} = testCase "base58" $ do
  let enc = B58.encode vb_decodedHex
  assertEqual mempty enc vb_encoded

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
        testGroup "base58" (fmap execute_base58 b58)
      , execute_base58check b58c
      ]

