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
  for_ (buildChangeBatch z) $ \change ->
    A.send $ A.changeResourceRecordSets (extractHostedZoneId $ view A.hzId hz) change

buildChangeBatch :: HostedZone -> Maybe A.ChangeBatch
buildChangeBatch z =
  fmap A.changeBatch . NEL.nonEmpty . flip fmap (batch . hostedZoneRecords $ z) $ \(r, d, t, rs) ->
    A.change A.Upsert (buildRecords r d t rs)

batch :: [RecordSet] -> [(A.RecordType, Domain, Ttl, [Record])]
batch rs =
  let
    sorted = L.sortOn (\x -> (recordSetName x, renderRecordType . recordSetDetail $ x)) rs
    grouped = L.groupBy (\x y -> (recordSetName x, renderRecordType . recordSetDetail $ x) == (recordSetName y, renderRecordType . recordSetDetail $ y)) sorted
  in
    join $ flip fmap grouped $ \group ->
      case group of
        [] ->
          []
        (h:t) ->
          [(buildRecordType . recordSetDetail $ h, recordSetName h, recordSetTtl h, fmap recordSetDetail (h:t))]

buildRecordType :: Record -> A.RecordType
buildRecordType r =
  case r of
    ARecord _ -> A.A
    AAAARecord _ -> A.Aaaa
    CNAMERecord _ -> A.Cname
    MXRecord _ _ -> A.MX
    NSRecord _ -> A.NS
    PTRRecord _ -> A.Ptr
    SPFRecord _ -> A.Spf
    SRVRecord _ _ _ _ -> A.Srv
    TXTRecord _ -> A.Txt

buildRecords :: A.RecordType -> Domain -> Ttl -> [Record] -> A.ResourceRecordSet
buildRecords r d t rs =
  A.resourceRecordSet (domain d) r &
    A.rrsTTL .~ (Just . fromInteger . toInteger . ttl $ t) &
    A.rrsResourceRecords .~ (NEL.nonEmpty $ fmap buildRecord rs)

buildRecord :: Record -> A.ResourceRecord
buildRecord r =
  A.resourceRecord $ case r of
    ARecord x -> resource x
    AAAARecord x -> resource x
    CNAMERecord x -> resource x
    MXRecord p x -> T.intercalate " " [renderPriority p, resource x]
    NSRecord x -> resource x
    PTRRecord x -> resource x
    SPFRecord x -> resource x
    SRVRecord p w t x -> T.intercalate " " [renderPriority p, renderWeight w, renderPort t, resource x]
    TXTRecord x -> renderQuoted x

--
-- Regrettably the API is terrible and you can't directly use
-- the output IDs as input without this warping.
--
-- See - https://github.com/brendanhay/amazonka/issues/318
--
extractHostedZoneId :: Text -> Text
extractHostedZoneId =
  T.replace "/hostedzone/" ""
