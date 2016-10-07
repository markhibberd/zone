{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Zone.Serial (
    toRecord
  , fromRecord
  , recordSetParser
  ) where

import qualified Data.Attoparsec.Text as P
import           Data.Char (isSpace)
import           Data.Text (Text)
import qualified Data.Text as T

import           Zone.Data
import           Zone.P

toRecord :: Text -> Either Text RecordSet
toRecord t =
  first (\msg -> mconcat ["Could not parse record, '", t, "', with: ", T.pack msg]) . P.parseOnly recordSetParser $ t

fromRecord :: RecordSet -> Text
fromRecord r =
  T.intercalate " " [
      domain . recordSetName $ r
    , renderRecordType . recordSetType $ r
    , renderTtl . recordSetTtl $ r
    , T.intercalate " " . fmap resource . recordSetResources $ r
    ]

recordSetParser :: P.Parser RecordSet
recordSetParser = do
  d <- Domain <$> P.takeWhile1 (not . isSpace)
  _ <- P.many1 P.space
  r <- P.choice [
      AAAARecord <$ P.string "AAAA"
    , ARecord <$ P.string "A"
    , CNAMERecord <$ P.string "CNAME"
    , MXRecord <$ P.string "MX"
    , TXTRecord <$ P.string "TXT"
    , PTRRecord <$ P.string "PTR"
    , SRVRecord <$ P.string "SRV"
    , SPFRecord <$ P.string "SPF"
    , NSRecord <$ P.string "NS"
    ]
  _ <- P.many1 P.space
  t <- Ttl <$> P.decimal
  _ <- P.many1 P.space
  rs <- P.sepBy1 (Resource <$> P.takeWhile1 (not . isSpace)) (P.many1 P.space)
  _ <- many P.space
  pure $ RecordSet d r t rs
