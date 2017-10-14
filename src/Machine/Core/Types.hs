module Machine.Core.Types(
  Program(..)
 ,Instruction(..)
 ,Machine(..)
 ,Result(..)
 ,Computer
) where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Vector
import Data.Map

newtype Program = Program { unP :: Vector Instruction }
  deriving Show

data Instruction =
    Zero Int
  | Inc  Int
  | Jump Int Int Int
  deriving Show

data Machine = Machine {
    memory         :: Vector Int
   ,programCounter :: Int
   ,operationCount :: Map Int Int
  } deriving Show

data Result =
    Running
  | Finished Int Machine
  deriving Show

type Computer = ReaderT (Program, Int) (StateT Machine (Except String)) Result
