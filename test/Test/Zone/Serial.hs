{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Zone.Serial where

import           Data.Text (Text)

import           System.IO (IO)

import           Test.QuickCheck
import           Test.Zone.Arbitrary ()

import           Zone.Data
import           Zone.P
import           Zone.Serial


prop_symmetic :: RecordSet -> Property
prop_symmetic r =
 (toRecord . fromRecord) r === Right r

prop_examples :: Property
prop_examples =
  conjoin [
      counterexample "Render not working as expected." $
        fromRecord (ex "sub.example.com." CNAMERecord 300 ["127.0.0.1"]) ===
          "sub.example.com. CNAME 300 127.0.0.1"

    , counterexample "Render with multiple resources not working as expected." $
        fromRecord (ex "sub.example.com." CNAMERecord 300 ["127.0.0.1", "127.0.0.2"]) ===
          "sub.example.com. CNAME 300 127.0.0.1 127.0.0.2"

    , counterexample "Parse not working as expected." $
        toRecord "sub.example.com. CNAME 300 127.0.0.1" ===
          Right (ex "sub.example.com." CNAMERecord 300 ["127.0.0.1"])

    , counterexample "Parse with multiple resources not working as expected." $
        toRecord "sub.example.com. CNAME 300 127.0.0.1 127.0.0.2" ===
          Right (ex "sub.example.com." CNAMERecord 300 ["127.0.0.1", "127.0.0.2"])
    ]

ex :: Text -> RecordType -> Int -> [Text] -> RecordSet
ex d r t rs =
  RecordSet (Domain d) r (Ttl t) (Resource <$> rs)

return []
tests :: IO Bool
tests =
  $quickCheckAll
