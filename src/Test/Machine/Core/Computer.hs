module Test.Machine.Core.Computer(
  tests
) where

import Machine.Core.Types
import Machine.Core.Computer
import Test.Machine.QuickCheck as Test

import Control.Monad
import Data.Either
import qualified Data.Vector as V
import qualified Data.Map as M
import Debug.Trace
import Text.Printf (printf)
import Test.QuickCheck hiding (Result)
import qualified Test.QuickCheck.Monadic as QC

tests :: TestSuite
tests = map (\(x, y) -> ("Computer - " ++ x, y))
  [
    ("Basic Op: Inc / Zero",          testBasicOp)
   ,("Copy Loop cell(1) -> cell(0)",  testCopyLoop)
   ,("Infinite Loop",                 testInfiniteLoop)
   ,("Rand Inc",                      testRandInc)
   ,("Machine initialised properly",  testMachineInit)
   ,("Performance",                   testPerfComputer)
   ,("Combine programs",              testCombinePrograms)
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
  TestQC $ run $ forAll (choose (1, memLimit - 1)) $ \ i ->
    let r = getResUnSafe $ runProgram $
          Zero 0 : Zero 1 : replicate 5 (Inc i) ++ [Inc 0, Jump 0 i 7]
     in r == 5

testRandInc :: Test
testRandInc =
  TestQC $ run $ forAll (abs `fmap` arbitrary) $ \ n ->
    let r = getResUnSafe $ runProgram $ replicate n $ Inc 0
     in n == r

testInfiniteLoop :: Test
testInfiniteLoop =
  TestPure $ const $
    isLeft $ runProgram [ Inc 0, Jump 0 1 0 ]

testMachineInit :: Test
testMachineInit =
  TestPure $ const $
    let m = mkMachine
    in 0 == V.foldl (\ acc x -> acc + x) 0 (memory m)
    && 0 == M.size (operationCount m)
    && 0 == programCounter m

testPerfComputer :: Test
testPerfComputer =
  TestQC $ Test.runWith 5 $ forAll (choose (1, memLimit - 1)) $
    \ i -> QC.monadicIO $ do
      let n = 1000000
      t <- QC.run $ time $ getResUnSafe $ runProgram $
              replicate n (Inc i) ++ [Inc 0, Jump 0 i n]

      let res = t < 8.5
      unless res $
        QC.run $ printf "Time : %0.9f sec" t
      QC.assert res

testCombinePrograms :: Test
testCombinePrograms =
  TestQC $ run $ forAll (fmap (\(x,y) -> (abs x, abs y)) arbitrary) $
    \ (i, j) -> let
        (Right (Finished _ m)) = runProgram $ replicate i $ Inc 0
        y = getResUnSafe $
          compute (Program $ V.fromList $ replicate j $ Inc 0)
            (Just m) Nothing
      in y == ( i + j )
