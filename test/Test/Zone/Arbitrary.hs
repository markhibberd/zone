{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Zone.Arbitrary where

import qualified Data.Text as T

import           Test.QuickCheck

import           Zone.Data
import           Zone.P

instance Arbitrary ZoneName where
  arbitrary = do
    a <- elements ["red", "green", "blue", "orange", "black", "purple", "pink", "white"]
    b <- elements ["io", "com", "net"]
    pure . ZoneName . mconcat $ [a, ".", b, "."]

instance Arbitrary Resource where
  arbitrary = do
    i <- choose (1 :: Int, 99)
    pure . Resource . mconcat $ ["127.0.0.", T.pack . show $ i]

instance Arbitrary Ttl where
  arbitrary =
    Ttl <$> choose (60, 3600)

instance Arbitrary RecordType where
  arbitrary =
    elements [minBound .. maxBound]

instance Arbitrary Domain where
  arbitrary = do
    z <- arbitrary
    x <- elements ["www", "blog", "dev", "internal"]
    pure . Domain . mconcat $ [x, ".", zoneName z]

instance Arbitrary RecordSet where
  arbitrary =
    RecordSet
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> (choose (1, 5) >>= flip vectorOf arbitrary)
