{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Zone.IO (
    LoadError (..)
  , load
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (ExceptT (..), throwE)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified System.Directory as D
import qualified System.FilePath as F
import           System.IO (IO, FilePath)

import           Zone.Data
import           Zone.P
import           Zone.Serial


data LoadError =
    FilenameExtensionError FilePath
  | FileNotFound FilePath
  | FileParseError ParseError

load :: FilePath -> ExceptT LoadError IO HostedZone
load f =
  let
    base = F.takeFileName f
    ext = F.takeExtension base
    name = F.dropExtension base
  in
    case ext == ".zone" of
      False ->
        throwE $ FilenameExtensionError f
      True -> do
        e <- liftIO $
          D.doesFileExist f
        unless e $
          throwE $ FileNotFound f
        txt <- liftIO $
          T.readFile f
        records <- ExceptT . pure . first FileParseError $
          parseFile txt
        pure $ HostedZone (ZoneName . T.pack $ name) records
