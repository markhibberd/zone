{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Zone.Data (
    ZoneName (..)
  , Resource (..)
  , Ttl (..)
  , RecordType (..)
  , Domain (..)
  , RecordSet (..)
  , HostedZone (..)
  , renderRecordType
  , renderTtl
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

import           Zone.P


newtype ZoneName =
  ZoneName {
      zoneName :: Text
    } deriving (Eq, Show, Ord)

newtype Resource =
  Resource {
      resource :: Text
    } deriving (Eq, Show, Ord)

newtype Ttl =
  Ttl {
      ttl :: Int
    } deriving (Eq, Show, Ord)

data RecordType =
    ARecord
  | CNAMERecord
  | MXRecord
  | AAAARecord
  | TXTRecord
  | PTRRecord
  | SRVRecord
  | SPFRecord
  | NSRecord
    deriving (Eq, Show, Enum, Bounded)

newtype Domain =
  Domain {
      domain :: Text
    } deriving (Eq, Show)

data RecordSet =
  RecordSet {
      recordSetName :: Domain
    , recordSetType :: RecordType
    , recordSetTtl :: Ttl
    , recordSetResources :: [Resource]
    } deriving (Eq, Show)

data HostedZone =
  HostedZone {
      hostedZoneName :: ZoneName
    , hostedZoneRecords :: [RecordSet]
    }

renderRecordType :: RecordType -> Text
renderRecordType r =
  case r of
    ARecord ->
      "A"
    CNAMERecord ->
      "CNAME"
    MXRecord ->
      "MX"
    AAAARecord ->
      "AAAA"
    TXTRecord ->
      "TXT"
    PTRRecord ->
      "PTR"
    SRVRecord ->
      "SRV"
    SPFRecord ->
      "SPF"
    NSRecord ->
      "NS"

renderTtl :: Ttl -> Text
renderTtl =
  T.pack . show . ttl
