{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Zone.Operation (
    apply
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Lens ((&), (.~), view)

import           Data.Conduit (($$))
import qualified Data.Conduit.List as CL
import           Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.List.NonEmpty as NEL

import           Network.AWS (AWS)
import qualified Network.AWS as A
import qualified Network.AWS.Route53 as A

import           Zone.Data
import           Zone.P

apply :: HostedZone -> AWS ()
apply z = do
  now <- liftIO getCurrentTime
  let
    name = (zoneName . hostedZoneName) z
    stamp = T.pack $ formatTime defaultTimeLocale "%Y%m%d%H%M%S" now
    token = mconcat [name, "-", stamp]
  zones <- A.paginate A.listHostedZones $$ CL.consume
  hz <- case L.find (\hz -> view A.hzName hz == name || view A.hzName hz == mconcat [name, "."]) (zones >>= view A.lhzrsHostedZones) of
    Nothing ->
      fmap (view A.chzrsHostedZone) . A.send $ A.createHostedZone name token
    Just hz ->
      pure hz
  for_ (buildChangeBatch z) $ \batch ->
    A.send $ A.changeResourceRecordSets (extractHostedZoneId $ view A.hzId hz) batch

buildChangeBatch :: HostedZone -> Maybe A.ChangeBatch
buildChangeBatch z =
  fmap A.changeBatch . NEL.nonEmpty . flip fmap (hostedZoneRecords z) $ \r ->
    A.change A.Upsert $
      A.resourceRecordSet
        (domain . recordSetName $ r)
        (buildRecordType . recordSetType $ r) &
          A.rrsTTL .~ (Just . fromInteger . toInteger . ttl . recordSetTtl $ r) &
          A.rrsResourceRecords .~ NEL.nonEmpty (fmap buildResourceRecord . recordSetResources $ r)

buildRecordType :: RecordType -> A.RecordType
buildRecordType r =
  case r of
    ARecord -> A.A
    AAAARecord -> A.Aaaa
    CNAMERecord -> A.Cname
    MXRecord -> A.MX
    NSRecord -> A.NS
    PTRRecord -> A.Ptr
    SPFRecord -> A.Spf
    SRVRecord -> A.Srv
    TXTRecord -> A.Txt

buildResourceRecord :: Resource -> A.ResourceRecord
buildResourceRecord =
  A.resourceRecord . resource

--
-- Regrettably the API is terrible and you can't directly use
-- the output IDs as input without this warping.
--
-- See - https://github.com/brendanhay/amazonka/issues/318
--
extractHostedZoneId :: Text -> Text
extractHostedZoneId =
  T.replace "/hostedzone/" ""
