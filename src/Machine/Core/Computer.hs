{-# LANGUAGE FlexibleContexts #-}
module Machine.Core.Computer(
  compute
 ,mkMachine
 ,memLimit
) where

import Machine.Core.Types

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Vector as V

compute :: Program -> Maybe Machine -> Maybe Int -> Either String Result
compute prog machineM opCountLimitM =
  let machine  = fromMaybe mkMachine machineM
      opCL     = fromMaybe (V.length (unP prog) + 1) opCountLimitM
      computer = mkComputer
      result   = runExcept $
                  flip runStateT machine $
                     flip runReaderT (prog, opCL)
                        computer
  in case result of
       Left err       -> Left err
       Right (res, m) -> case res of
         Running  -> compute prog (Just m) $ Just opCL
         _        -> Right res

mkMachine :: Machine
mkMachine = Machine {
    memory         = V.fromList $ replicate memLimit 0
   ,programCounter = 0
   ,operationCount = M.empty
 }

memLimit :: Int
memLimit = 32

mkComputer :: Computer
mkComputer = do
  (Program is, opCountLim) <- ask
  m@(Machine cells pC oC) <- get
  if pC >= V.length is
     then return $ Finished (cells V.! 0) m
     else do
       when (pC `M.member` oC && oC M.! pC == opCountLim) $
          throwError "Loop detected"
       let i = is V.! pC
           updateMachine :: (MonadState Machine m, MonadError String m) =>
              Int -> Int -> m()
           updateMachine x v = do
             when (x < 0 || x > (memLimit - 1)) $
               throwError $ "Cell Id out of memory bounds, pC: " ++ show pC ++
                 " -Id: " ++ show x
             put m{
                  memory = cells V.// [(x, v)]
                 ,programCounter = pC + 1
                 ,operationCount = M.insertWith (+) pC 1 oC
                }
       case i of
         Zero x     -> updateMachine x 0
         Inc x      -> updateMachine x $ cells V.! x + 1
         Jump x y t -> do
           when (x < 0 || x > (memLimit - 1) ||
                 y < 0 || y > (memLimit - 1)) $
             throwError $ "Cell Id out of memory bound, pC: " ++ show pC ++
                 " -Id X: " ++ show x ++
                 " -Id Y: " ++ show y
           when (t < 0 || t > (V.length is - 1) ) $
             throwError $ "Operation Id out of bound, pC: " ++ show pC
           when (t == pC) $
             throwError $ "Jump at the same place ! pC: " ++ show pC
           put m{
               programCounter = if cells V.! x /= cells V.! y then t else pC + 1
              ,operationCount = M.insertWith (+) pC 1 oC
             }
       return Running
