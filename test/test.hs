{-# LANGUAGE NoImplicitPrelude #-}

import           System.IO (IO, hSetBuffering, BufferMode (..), stdout)
import           System.Exit (exitFailure)

import qualified Test.Zone.Serial

import           Zone.P

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering >> mapM id [
      Test.Zone.Serial.tests
    ] >>= \rs -> when (not . all id $ rs) exitFailure
