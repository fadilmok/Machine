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
    (Result r _) <- foldM (run opLimit) (Result (-1) mkMachine) files
    return r
  putStrLn $ case res of
    Left err -> "There was an error: " ++ err
    Right r  -> "The run is successful, the result is: " ++ show r

-- | Run one program at the time, using the previous state of the machine
run :: Int -> Result -> FilePath -> ExceptT String IO Result
run oL (Result _ m) f = do
  program <- loadProgram f
  lift $ putStrLn $ "Running program " ++ f
  compute program (Just m) $ Just oL
