module Main where

import Data.Aeson ((.:))
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base58 as B58
import qualified Data.ByteString.Base58Check as B58Check
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

toBS :: T.Text -> BS.ByteString
toBS = B16.decodeLenient . TE.encodeUtf8

data Valid_Base58Check = Valid_Base58Check {
    vc_string  :: !BS.ByteString
  , vc_payload :: !BS.ByteString
  } deriving Show

instance A.FromJSON Valid_Base58Check where
  parseJSON = A.withObject "Valid_Base58Check" $ \m -> Valid_Base58Check
    <$> fmap toBS (m .: "string")
    <*> fmap toBS (m .: "payload")

data Invalid_Base58Check = Invalid_Base58Check {
    ic_string  :: !BS.ByteString
  } deriving Show

instance A.FromJSON Invalid_Base58Check where
  parseJSON = A.withObject "Invalid_Base58Check" $ \m -> Invalid_Base58Check
    <$> fmap toBS (m .: "string")

data Valid_Base58 = Valid_Base58 {
    vb_decodedHex  :: !BS.ByteString
  , vb_encoded :: !BS.ByteString
  } deriving Show

instance A.FromJSON Valid_Base58 where
  parseJSON = A.withObject "Valid_Base58" $ \m -> Valid_Base58
    <$> fmap toBS (m .: "decodedHex")
    <*> fmap toBS (m .: "encoded")

main = pure ()
