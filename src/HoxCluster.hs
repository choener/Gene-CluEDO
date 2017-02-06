
module Main where

import System.Environment (getArgs)
import Control.Monad (forM_)

import BioInf.HoxCluster



main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \a -> do
    test a

