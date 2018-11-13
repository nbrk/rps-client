module Main where

import           UI                           (startUI)

import           RPS.Types
import           RPS.TypesInstances

import           WEGO

import           Control.Concurrent.STM.TChan


-------------------------

main :: IO ()
main = do
  putStrLn "Rock/Paper/Scissors client"

  -- (ctrlc, viewc) <-
  --   spawnClient "localhost" ("20000", "30000") :: IO (TChan Patch, TChan Patch)

  startUI
  return ()
