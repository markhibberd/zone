{-# LANGUAGE NoImplicitPrelude #-}
module Zone.Operation (
    apply
  ) where

import           Control.Lens (view)

import           Data.Conduit (($$))
import qualified Data.Conduit.List as CL
import qualified Data.List as L

import           Network.AWS (AWS)
import qualified Network.AWS as A
import qualified Network.AWS.Route53 as A

import           Zone.Data
import           Zone.P

apply :: HostedZone -> AWS ()
apply z = do
  let name = (zoneName . hostedZoneName) z
  all <- A.paginate A.listHostedZones $$ CL.consume
  hz <- case L.find (\hz -> view A.hzName hz == name) (all >>= view A.lhzrsHostedZones) of
    Nothing ->
      fmap (view A.chzrsHostedZone) . A.send $ A.createHostedZone name name
    Just hz ->
      pure hz
  void . A.send $ A.changeResourceRecordSets (view A.hzId hz) (buildChangeBatch z)


buildChangeBatch :: HostedZone -> A.ChangeBatch
buildChangeBatch =
 _blah
