-- | Module providing the tool to run the tets
module Test.Machine.QuickCheck(
  -- * Run
  run, runWith, runTest,
  -- * Constants
  nTests, nRand,
  -- * Tools
  failTest, time,
  -- * Type
  TestSuite, Test(..)
) where

import Machine.Core.Types

import Control.Exception
import Control.DeepSeq
import Control.Monad
import qualified Data.Map as Map
import System.CPUTime
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)

-- | TestSuite type
type TestSuite = [(String, Test)]
data Test =
    TestQC                    (IO Bool)
  | TestPure                  (() -> Bool)
  | TestIO                    (IO Bool)

runTest :: Test -> IO Bool
runTest (TestQC t) = t
runTest (TestPure t) = do
  let res = t()
  putStrLn $ " Test " ++ if res then "Passed" else "Failed"
  return res
runTest (TestIO t) = do
  res <- t
  putStrLn $ " Test " ++ if res then "Passed" else "Failed"
  return res

-- | Number of quickcheck tests
nTests :: Int
nTests = 1000

-- | Number of random number to simulate in some tests
nRand :: Int
nRand = 100000

-- | Run a quickcheck test nTests times
run :: Testable prop => prop -> IO Bool
run = runWith nTests

-- | Run a quickcheck test n times
runWith :: Testable prop => Int -> prop -> IO Bool
runWith n p = fmap isSuccess $
  quickCheckWithResult stdArgs{ maxSuccess = n } p

-- | Test if the input function raises an exception
failTest :: a -> IO Bool
failTest f = do
  res' <- try $ do
        evaluate f
        return False

  let res = case (res' :: Either SomeException Bool) of
              Left e -> True
              Right _ -> False
  return res

-- | Time an IO action
time :: NFData t => t -> IO Double
time f = do
  start <- getCPUTime
  x <- evaluate f
  rnf x `seq` return()
  end <- getCPUTime
  return $ fromIntegral (end - start) / 10^12

