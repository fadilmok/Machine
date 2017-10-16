module Test.Machine.Loader(
  tests
) where

import Machine.Loader
import Machine.Core.Computer
import Machine.Core.Types
import Test.Machine.QuickCheck as Test

import Control.Monad.Except
import Data.Either
import qualified Data.Vector as V
import Test.QuickCheck hiding (Result)

tests :: TestSuite
tests = map (\ (x, y) -> ("Loader - " ++ x, y))
  [
    ("Parse Inst Success",  testParseInstr),
    ("Parse Inst Rand",     testRandParseInstr),
    ("Parse Inst Fail",     testParseInstrFail)
  ]

parseP :: [String] -> Either String Program
parseP = runExcept . parseProgram

mkP :: [Instruction] -> Program
mkP = Program . V.fromList

testParseInstr :: Test
testParseInstr =
  TestPure $ const $
    parseP ["i1", "z0","j1,2,1"] == Right (mkP [Inc 1, Zero 0, Jump 1 2 1])  &&
    parseP ["i 1", "z 0","j 1, 20,1"] == Right (mkP [Inc 1, Zero 0, Jump 1 20 1])

testRandParseInstr :: Test
testRandParseInstr =
  TestQC $ runWith 50 $ forAll (choose (0, memLimit - 1)) $ \ n ->
    parseP ["i" ++ show n] == Right (mkP [Inc n])

testParseInstrFail :: Test
testParseInstrFail =
  TestPure $ const $
    isLeft (parseP ["i -1"]) &&
      isLeft (parseP ["j0,1,0"]) &&
        isLeft (parseP ["I \2"])
