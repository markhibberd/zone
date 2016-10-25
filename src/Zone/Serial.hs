{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Zone.Serial (
    ParseError (..)
  , parseFile
  , toRecord
  , fromRecord
  , recordSetParser
  , recordSetParser'
  , domainParser
  , resourceParser
  , priorityParser
  , weightParser
  , portParser
  , ttlParser
  , quotedParser
  , tokenize
  ) where

import qualified Data.Attoparsec.Text as P
import           Data.Char (isSpace)
import           Data.Text (Text)
import qualified Data.Text as T

import           Zone.Data
import           Zone.P

data ParseError =
    ParseError Text Text

parseFile :: Text -> Either ParseError [RecordSet]
parseFile t =
  fmap join . for (T.lines t) $ \l ->
    case "#" `T.isPrefixOf` l of
      True ->
        pure []
      False ->
        bimap (ParseError l) pure $
          toRecord l

toRecord :: Text -> Either Text RecordSet
toRecord t =
  first (\msg -> mconcat ["Could not parse record, '", t, "', with: ", T.pack msg]) . P.parseOnly recordSetParser $ t

fromRecord :: RecordSet -> Text
fromRecord r =
  case recordSetDetail r of
    ARecord x ->
      T.intercalate " " ["A", renderTtl . recordSetTtl $ r, domain . recordSetName $ r, resource x]
    CNAMERecord x ->
      T.intercalate " " ["CNAME", renderTtl . recordSetTtl $ r, domain . recordSetName $ r, resource x]
    MXRecord p x ->
      T.intercalate " " ["MX", renderTtl . recordSetTtl $ r, domain . recordSetName $ r, renderPriority p, resource x]
    AAAARecord x ->
      T.intercalate " " ["AAAA", renderTtl . recordSetTtl $ r, domain . recordSetName $ r, resource x]
    TXTRecord q ->
      T.intercalate " " ["TXT", renderTtl . recordSetTtl $ r, domain . recordSetName $ r, renderQuoted q]
    PTRRecord x ->
      T.intercalate " " ["PTR", renderTtl . recordSetTtl $ r, domain . recordSetName $ r, resource x]
    SRVRecord p w t x ->
      T.intercalate " " ["SRV", renderTtl . recordSetTtl $ r, domain . recordSetName $ r, renderPriority p, renderWeight w, renderPort t, resource x]
    SPFRecord x ->
      T.intercalate " " ["SPF", renderTtl . recordSetTtl $ r, domain . recordSetName $ r, resource x]
    NSRecord x ->
      T.intercalate " " ["NS", renderTtl . recordSetTtl $ r, domain . recordSetName $ r, resource x]

recordSetParser :: P.Parser RecordSet
recordSetParser = do
  P.choice [
      recordSetParser' "AAAA" $ AAAARecord <$> tokenize resourceParser
    , recordSetParser' "A" $ ARecord <$> tokenize resourceParser
    , recordSetParser' "CNAME" $ CNAMERecord <$> tokenize resourceParser
    , recordSetParser' "MX" $ MXRecord <$> tokenize priorityParser <*> tokenize resourceParser
    , recordSetParser' "TXT" $ TXTRecord <$> tokenize quotedParser
    , recordSetParser' "PTR" $ PTRRecord <$> tokenize resourceParser
    , recordSetParser' "SRV" $ SRVRecord <$> tokenize priorityParser <*> tokenize weightParser <*> tokenize portParser <*> tokenize resourceParser
    , recordSetParser' "SPF" $ SPFRecord <$> tokenize resourceParser
    , recordSetParser' "NS" $ NSRecord <$> tokenize resourceParser
    ]

recordSetParser' :: Text -> P.Parser Record -> P.Parser RecordSet
recordSetParser' x p = do
  _ <- P.string x
  t <- tokenize ttlParser
  d <- tokenize domainParser
  r <- p
  pure $ RecordSet d t r

domainParser :: P.Parser Domain
domainParser =
  Domain <$> P.takeWhile1 (not . isSpace)

resourceParser :: P.Parser Resource
resourceParser =
  Resource <$> P.takeWhile1 (not . isSpace)

priorityParser :: P.Parser Priority
priorityParser =
  Priority <$> P.decimal

weightParser :: P.Parser Weight
weightParser =
  Weight <$> P.decimal

portParser :: P.Parser Port
portParser =
  Port <$> P.decimal

ttlParser :: P.Parser Ttl
ttlParser =
  Ttl <$> P.decimal

quotedParser :: P.Parser Quoted
quotedParser =
   quote *> (Quoted <$> P.takeWhile1 (not . (==) '"')) <* quote

quote :: P.Parser ()
quote =
  void $ P.string "\""

tokenize :: P.Parser a -> P.Parser a
tokenize p =
  P.many1 P.space >> p
