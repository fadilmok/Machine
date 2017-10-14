module Test.Machine.Core.Computer(
  tests
) where

import qualified Data.Vector as V
import qualified Data.Map as M

import Machine.Core.Types
import Machine.Core.Computer
import Test.Machine.QuickCheck
import Test.QuickCheck hiding (Result)
import Debug.Trace

tests :: TestSuite
tests = map (\(x, y) -> ("Computer - " ++ x, y))
  [
    ("Basic Op: Inc / Zero",          testBasicOp)
   ,("Copy Loop cell(1) -> cell(0)",  testCopyLoop)
   ,("Infinite Loop",                 testInfiniteLoop)
   ,("Rand Inc",                      testRandInc)
   ,("Machine initialised properly",  testMachineInit)
  ]

runProgram :: [Instruction] -> Either String Result
runProgram p = compute (Program $ V.fromList p) Nothing Nothing

getResUnSafe :: Either String Result -> Int
getResUnSafe (Right (Finished x _)) = x
getResUnSafe (Left s) = trace s $ -1

testBasicOp :: Test
testBasicOp =
  TestPure $ const $
    zero == 0 && inc == 1
  where
    zero = getResUnSafe $ runProgram [ Zero 0 ]
    inc = getResUnSafe $ runProgram [ Inc 0 ]

testCopyLoop :: Test
testCopyLoop =
  TestPure $ const $
    r == 5
  where
    r = getResUnSafe $ runProgram $
      Zero 0 : Zero 1 : replicate 5 (Inc 1) ++ [Inc 0, Jump 0 1 7]

testRandInc :: Test
testRandInc =
  TestQC $ run $ forAll (abs `fmap` arbitrary) $ \ n ->
    let r = getResUnSafe $ runProgram $ replicate n $ Inc 0
     in n == r

testInfiniteLoop :: Test
testInfiniteLoop =
  TestPure $ const $
    r == "Loop detected"
  where
    (Left r) = runProgram [ Inc 0, Jump 0 1 0 ]

testMachineInit :: Test
testMachineInit =
  TestPure $ const $
    let m = mkMachine
    in 0 == V.foldl (\ acc x -> acc + x) 0 (memory m)
    && 0 == M.size (operationCount m)
    && 0 == programCounter m
