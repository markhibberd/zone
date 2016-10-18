{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Trans.Except (runExceptT)

import           Data.String (String)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Network.AWS as A
import qualified Network.AWS.Data as A

import           Options.Applicative (Parser, Mod, CommandFields)
import           Options.Applicative (customExecParser, execParser, prefs, showHelpOnError, info, helper, progDesc, idm)
import           Options.Applicative (subparser, command, flag', strArgument)
import           Options.Applicative (metavar, long, help)

import           System.Environment (getArgs, lookupEnv)
import           System.Exit (exitSuccess, exitFailure)
import           System.IO (IO, FilePath, putStrLn)
import           System.IO (BufferMode (..), hSetBuffering, stdout, stderr)

import           Zone.P
import qualified Zone.IO as Z
import qualified Zone.Operation as Z
import qualified Zone.Serial as Z

data Command =
    ApplyCommand FilePath
  | CheckCommand FilePath
  | VersionCommand
    deriving (Eq, Show)

zone :: Parser Command
zone =
  asum [
      subparser $ command' "apply" "apply zone" (ApplyCommand <$> zonefile)
    , subparser $ command' "check" "check zone" (CheckCommand <$> zonefile)
    , flag' VersionCommand $ mconcat [long "version", help "Version information."]
    ]

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  dispatch zone >>= \cmd -> case cmd of
    ApplyCommand f -> do
      runExceptT (Z.load f) >>= \e -> case e of
        Left err -> do
          T.hPutStrLn stderr . mconcat $ case err of
            Z.FilenameExtensionError x ->
              ["zone file [", T.pack x, "] must follow hosted filename convention of `{hosted-zone-name}.zone`."]
            Z.FileNotFound x ->
              ["zone file [", T.pack x, "] could not be found."]
            Z.FileParseError (Z.ParseError line msg) ->
              ["zone file [", T.pack f, "] could not be parsed, line [", line, "] failed with error: ", msg]
          exitFailure
        Right z -> do
          e <- aws
          A.runResourceT . A.runAWS e $
            Z.apply z
          exitSuccess

    CheckCommand f -> do
      runExceptT (Z.load f) >>= \e -> case e of
        Left err -> do
          T.hPutStrLn stderr . mconcat $ case err of
            Z.FilenameExtensionError x ->
              ["zone file [", T.pack x, "] must follow hosted filename convention of `{hosted-zone-name}.zone`."]
            Z.FileNotFound x ->
              ["zone file [", T.pack x, "] could not be found."]
            Z.FileParseError (Z.ParseError line msg) ->
              ["zone file [", T.pack f, "] could not be parsed, line [", line, "] failed with error: ", msg]
          exitFailure
        Right _ ->
          exitSuccess

    VersionCommand ->
      putStrLn "zone: 0.0.1"


dispatch :: Parser a -> IO a
dispatch p =
  getArgs >>= \x -> case x of
    [] -> customExecParser (prefs showHelpOnError) (info (p <**> helper) idm)
    _  -> execParser (info (p <**> helper) idm)

command' :: String -> String -> Parser a -> Mod CommandFields a
command' label description parser =
    command label (info (parser <**> helper) (progDesc description))

aws :: IO A.Env
aws = do
  rr <- lookupEnv "AWS_DEFAULT_REGION"
  let r = rr >>= either (const Nothing) Just . A.fromText . T.pack
  A.newEnv (fromMaybe A.NorthVirginia r) A.Discover

zonefile :: Parser FilePath
zonefile =
  strArgument . mconcat $ [
      metavar "ZONE_FILE_PATH"
    , help "zone file path, base filename used as hosted zone name, e.g. example.com.zone equates to hosted zone example.com."
    ]
