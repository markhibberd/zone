{-# LANGUAGE NoImplicitPrelude #-}
module Zone.Operation (
    apply
  ) where

import           Control.Lens ((&), (.~), view)

import           Data.Conduit (($$))
import qualified Data.Conduit.List as CL
import qualified Data.List as L
import qualified Data.List.NonEmpty as NEL

import           Network.AWS (AWS)
import qualified Network.AWS as A
import qualified Network.AWS.Route53 as A

import           Zone.Data
import           Zone.P

apply :: HostedZone -> AWS ()
apply z = do
  let name = (zoneName . hostedZoneName) z
  zones <- A.paginate A.listHostedZones $$ CL.consume
  hz <- case L.find (\hz -> view A.hzName hz == name) (zones >>= view A.lhzrsHostedZones) of
    Nothing ->
      fmap (view A.chzrsHostedZone) . A.send $ A.createHostedZone name name
    Just hz ->
      pure hz
  for_ (buildChangeBatch z) $ \batch ->
    A.send $ A.changeResourceRecordSets (view A.hzId hz) batch

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
