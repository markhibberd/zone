{-# LANGUAGE NoImplicitPrelude #-}
module Zone.Data (
    ZoneName (..)
  , Resource (..)
  , Ttl (..)
  , RecordType (..)
  , RecordSet (..)
  , HostedZone (..)
  ) where

import           Data.Text (Text)

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
  | NAPTRRecord
  | NSRecord

data RecordSet =
  RecordSet {
      recordSetTtl :: Ttl
    , recordSetResources :: [Resource]
    } deriving (Eq, Show)

data HostedZone =
  HostedZone {
      hostedZoneName :: ZoneName
    , hostedZoneRecords :: [RecordSet]
    }
