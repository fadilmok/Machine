{-# OPTIONS_GHC -O2 #-}
-- | Module to run all the testsuites
module Test.Machine.Run (
  main
) where

import Control.Monad
import System.Exit (exitFailure)
import Text.Printf
import Test.QuickCheck

import Test.Machine.QuickCheck (runTest)
import qualified Test.Machine.Core.Computer as Computer
import qualified Test.Machine.Loader as Loader

-- | Main to run the testsuite, does not stop for failure
main :: IO()
main = do
  success <- forM (
      Loader.tests ++
      Computer.tests
   ) $ \ (name, test) -> do
           printf "%-45s:" name
           runTest test

  unless (all (==True) success)
    exitFailure

