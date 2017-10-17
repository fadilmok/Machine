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

data Result = Result Int Machine
  deriving Show

-- | A computer which has the program and the operation count limit
-- in a reader monad, the state is its machine and m is the error
-- handling scheme, return a result.
type Computer m = ReaderT (Program, Int) (StateT Machine m) Result
