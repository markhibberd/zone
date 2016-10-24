{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Zone.Data (
    ZoneName (..)
  , Resource (..)
  , Ttl (..)
  , Priority (..)
  , Weight (..)
  , Port (..)
  , Quoted (..)
  , Record (..)
  , Domain (..)
  , RecordSet (..)
  , HostedZone (..)
  , renderRecordType
  , renderTtl
  , renderPriority
  , renderWeight
  , renderPort
  , renderQuoted
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

newtype Priority =
  Priority {
      priority :: Int
    } deriving (Eq, Show, Ord)

newtype Weight =
  Weight {
      weight :: Int
    } deriving (Eq, Show, Ord)

newtype Port =
  Port {
      port :: Int
    } deriving (Eq, Show, Ord)

newtype Quoted =
  Quoted {
      quoted :: Text
    } deriving (Eq, Show, Ord)

data Record =
    ARecord Resource
  | CNAMERecord Resource
  | MXRecord Priority Resource
  | AAAARecord Resource
  | TXTRecord Quoted
  | PTRRecord Resource
  | SRVRecord Priority Weight Port Resource
  | SPFRecord Resource
  | NSRecord Resource
    deriving (Eq, Show)

newtype Domain =
  Domain {
      domain :: Text
    } deriving (Eq, Show, Ord)

data RecordSet =
  RecordSet {
      recordSetName :: Domain
    , recordSetTtl :: Ttl
    , recordSetDetail :: Record
    } deriving (Eq, Show)

data HostedZone =
  HostedZone {
      hostedZoneName :: ZoneName
    , hostedZoneRecords :: [RecordSet]
    }

renderRecordType :: Record -> Text
renderRecordType r =
  case r of
    ARecord _ ->
      "A"
    CNAMERecord _ ->
      "CNAME"
    MXRecord _ _ ->
      "MX"
    AAAARecord _ ->
      "AAAA"
    TXTRecord _ ->
      "TXT"
    PTRRecord _ ->
      "PTR"
    SRVRecord _ _ _ _ ->
      "SRV"
    SPFRecord _ ->
      "SPF"
    NSRecord _ ->
      "NS"

renderTtl :: Ttl -> Text
renderTtl =
  T.pack . show . ttl

renderPriority :: Priority -> Text
renderPriority =
  T.pack . show . priority

renderWeight :: Weight -> Text
renderWeight =
  T.pack . show . weight

renderPort :: Port -> Text
renderPort =
  T.pack . show . port

renderQuoted :: Quoted -> Text
renderQuoted q =
  mconcat ["\"", quoted q, "\""]
