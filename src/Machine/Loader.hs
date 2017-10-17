{-# LANGUAGE FlexibleContexts #-}
module Machine.Loader(
  loadProgram
 ,checkFiles
 ,parseProgram
 ,parseInstruction
) where

import Machine.Core.Types
import Machine.Core.Computer

import Control.Monad
import Control.Monad.Except
import qualified Data.Text as T
import Data.Char
import Data.List
import qualified Data.Vector as V
import System.Directory

checkFiles :: [FilePath] -> ExceptT String IO Int
checkFiles files = do
  programLengths <- forM files $ \ f -> do
    eF <- lift $ doesFileExist f
    unless eF $
      throwError $ "File " ++ f ++ " doesn't exist."
    is <- fmap lines $ lift $ readFile f
    withExceptT (\ e -> "In file: "++ f ++ " - "++ e) $
      parseProgram is
    return $ length is
  return $ (sum programLengths + 1) ^ complexityAllowed

loadProgram :: FilePath -> ExceptT String IO Program
loadProgram f = do
  eF <- lift $ doesFileExist f
  unless eF $
    throwError $ "File " ++ f ++ " doesn't exist."
  is <- fmap lines $ lift $ readFile f
  withExceptT (\ e -> "In file: "++ f ++ " - " ++ e) $
    parseProgram is

parseProgram :: MonadError String m => [String] -> m Program
parseProgram is = do
  let pL = length is
  instrs <- mapM (\(inst, oC) -> parseInstruction inst pL oC) $
      zip is [0..]
  return $ Program $ V.fromList instrs

parseInstruction :: MonadError String m => String -> Int -> Int -> m Instruction
parseInstruction s pL oC = do
  let inst = map toUpper $ dropWhile (== ' ') s
  unless (all (==True) $ map isAscii inst) $
    throwError $ "Non ascii character detected in inst: " ++ show oC
  let test x = isDigit x || x == ' '
  i <- case inst of
    ('Z':xs) -> do
      unless (all (==True) $ map test xs) $
        throwError $ "Non digit character detected in inst: " ++ show oC
      return $ Zero $ read xs
    ('I':xs) -> do
      unless (all (==True) $ map test xs) $
        throwError $ "Non digit character detected in inst: " ++ show oC
      return $ Inc $ read xs
    ('J':xs) -> do
      unless (all (==True) $ map (\ x -> test x || x == ',') xs) $
        throwError $ "Non digit character detected in inst: " ++ show oC
      let ys = map T.unpack $ T.splitOn (T.pack ",") $ T.pack xs
      unless (length ys == 3) $
        throwError $ "Cannot parse inst : " ++ show oC
      return $ Jump (read $ ys!!0) (read $ ys!!1) $ read $ ys!!2
    _ -> throwError $ "Non recognised operation, id: " ++ show oC
  checkInstr i pL oC
  return i
