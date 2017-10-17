{-# LANGUAGE BangPatterns #-}
module Machine.Core.Types(
  Program(..)
 ,Instruction(..)
 ,Machine(..)
 ,Result(..)
 ,Computer
) where

import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Except
import Data.Vector
import Data.Map.Strict

newtype Program = Program { unP :: Vector Instruction }
  deriving (Show, Eq)

data Instruction =
    Zero {-# UNPACK #-} !Int
  | Inc  {-# UNPACK #-} !Int
  | Jump {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Show, Eq)

data Machine = Machine {
    memory         :: !(Vector Int)
   ,programCounter :: !Int
   ,operationCount :: !(Map Int Int)
  } deriving Show

data Result =
    Running
  | Finished Int Machine
  deriving Show

type Computer = ReaderT (Program, Int) (StateT Machine (Except String)) Result
