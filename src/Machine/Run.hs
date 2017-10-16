{-# OPTIONS_GHC -O2 #-}
module Machine.Run (
  main
) where

import Machine.Loader
import Machine.Core.Types
import Machine.Core.Computer

import Control.Monad
import Control.Monad.Except
import System.Environment

-- | Main function to run the program
main :: IO()
main = do
  files <- getArgs
  res <- runExceptT $ do
    when (length files < 1) $
      throwError "At least one file need to be passed as argument."
    opLimit <- checkFiles files
    (_, r) <- foldM (run opLimit) (mkMachine, -1) files
    return r
  putStrLn $ case res of
    Left err -> "There was an error: " ++ err
    Right r  -> "The run is successful, the result is: " ++ show r

run :: Int -> (Machine, Int) -> FilePath -> ExceptT String IO (Machine, Int)
run oL (m, _) f = do
  program <- loadProgram f
  lift $ putStrLn $ "Running program " ++ f
  let res = compute program (Just m) $ Just oL
  case res of
    Left err              -> throwError err
    Right Running         -> throwError "The program has not run completely"
    Right (Finished r m') -> return (m', r)
