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
   ,("Substraction",                  testSubstract)
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

      let res = t < 7.5
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

testSubstract :: Test
testSubstract = TestQC $
  run $ forAll (fmap (\(x,y) -> (abs x, abs y)) arbitrary
        `suchThat` (\(x, y) -> y < x) ) $
    \ (i, j) -> let
      a = 3; b = 4; c = 30; z = 31; w = 5; v = 6; t = 7
      x = 1; y = 2; r = 0
      m = i + j
      res = getResUnSafe $ runProgram $
          replicate i (Inc x) ++
          replicate j (Inc y) ++
            [
              Zero a, Zero b, Zero r,           -- m | initialisations
              Zero z, Zero c, Inc c,            -- m + 3 | z = 0, c = 1  constants
              Jump x z $ m + 8,                 -- m + 6 | if x /= 0 skip jump
              Jump z c $ m + 28,                -- m + 7 | skip all
                Jump y z $ m + 10,              -- m + 8 | if y /= 0 skip jump
                Jump z c $ m + 28,              -- m + 9 | skip decr x
                  Zero w, Zero v, Zero t,       -- m + 10 | y = 0, v = 0, t = 0
                  Jump x z $ m + 15,            -- m + 13 | if x /= 0 skip jump
                  Jump z c $ m + 24,            -- m + 14 | skip loop
                    Jump v z $ m + 17,          -- m + 15 | if v /= 0 skip jump
                    Jump c z $ m + 19,          -- m + 16 | skip loop
                    Inc w,                      -- m + 17 | w++
                    Jump w v $ m + 17,          -- m + 18 | if w /= v y++
                  Inc v,                        -- m + 19 | v++
                  Inc t,                        -- m + 20 | t++
                  Jump t x $ m + 17,            -- m + 21 | if t /= x decr (x)
                Zero x,                         -- m + 22 | x = 0
                Inc x,                          -- m + 23 | x++
                Jump x w $ m + 23,              -- m + 24 | if x /= w x++
              Inc a,                            -- m + 25 | a++
              Jump y a $ m + 10,                -- m + 26 | if y /= a decr x
              Inc r,                            -- m + 27 | r++
              Jump r x $ m + 27                 -- m + 28 | if x /= r x++
            ]
    in res == (i - j)

