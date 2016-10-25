{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Zone.Serial where

import qualified Data.Attoparsec.Text as P
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
        fromRecord (ex "sub.example.com." 300 . AAAARecord . Resource $ "127.0.0.1") ===
          "AAAA 300 sub.example.com. 127.0.0.1"

    , counterexample "Parse not working as expected." $
        toRecord "AAAA 300 sub.example.com. 127.0.0.1" ===
          Right (ex "sub.example.com." 300 . AAAARecord . Resource $ "127.0.0.1")
    ]

prop_symmetic_domain :: Domain -> Property
prop_symmetic_domain x =
  (P.parseOnly domainParser . domain) x === Right x

prop_symmetic_resource :: Resource -> Property
prop_symmetic_resource x =
  (P.parseOnly resourceParser . resource) x === Right x

prop_symmetic_priority :: Priority -> Property
prop_symmetic_priority x =
  (P.parseOnly priorityParser . renderPriority) x === Right x

prop_symmetic_weight :: Weight -> Property
prop_symmetic_weight x =
  (P.parseOnly weightParser . renderWeight) x === Right x

prop_symmetic_port :: Port -> Property
prop_symmetic_port x =
  (P.parseOnly portParser . renderPort) x === Right x

prop_symmetic_ttl :: Ttl -> Property
prop_symmetic_ttl x =
  (P.parseOnly ttlParser . renderTtl) x === Right x

prop_symmetic_quoted :: Quoted -> Property
prop_symmetic_quoted x =
  (P.parseOnly quotedParser . renderQuoted) x === Right x

prop_symmetic_tokenize :: Domain -> Property
prop_symmetic_tokenize x =
  forAll (elements [" ", "  ", "   ", "    "]) $ \s ->
    (P.parseOnly (tokenize domainParser) . (<>) s . domain) x === Right x

prop_symmetic_a_ttl :: Ttl -> Property
prop_symmetic_a_ttl t =
  P.parseOnly (P.string "A" >> tokenize ttlParser) (mconcat ["A ", renderTtl t]) === Right t

prop_symmetic_a_ttl_domain :: Ttl -> Domain -> Property
prop_symmetic_a_ttl_domain t d =
  P.parseOnly (do
    _ <- P.string "A"
    tt <- tokenize ttlParser
    dd <- tokenize domainParser
    pure (tt, dd)) (mconcat ["A ", renderTtl t, " ", domain d]) === Right (t, d)

prop_symmetic_a_ttl_domain_resource :: Ttl -> Domain -> Resource -> Property
prop_symmetic_a_ttl_domain_resource t d r =
  P.parseOnly (do
    _ <- P.string "A"
    tt <- tokenize ttlParser
    dd <- tokenize domainParser
    rr <- tokenize resourceParser
    pure (tt, dd, rr)) (mconcat ["A ", renderTtl t, " ", domain d, " ", resource r]) === Right (t, d, r)

prop_symmetic_record_set_a :: Domain -> Ttl -> Resource -> Property
prop_symmetic_record_set_a d t r =
    (P.parseOnly (recordSetParser' "A" $ ARecord <$> tokenize resourceParser) . fromRecord)  (RecordSet d t (ARecord r)) ===
      Right (RecordSet d t (ARecord r))

ex :: Text -> Int -> Record -> RecordSet
ex d t r =
  RecordSet (Domain d) (Ttl t) r

return []
tests :: IO Bool
tests =
  $quickCheckAll
