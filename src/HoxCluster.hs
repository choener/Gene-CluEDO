
module Main where

import System.Console.CmdArgs
import System.FilePath

import BioInf.HoxCluster



data Options = Options
  { infile      :: FilePath
  , outprefix   :: Maybe String
  , temperature :: Double
  , fillweight  :: FillWeight
  , fillstyle   :: FillStyle
  }
  deriving (Show,Data,Typeable)

oOptions = Options
  { infile      = def &= args
  , outprefix   = Nothing &= help "prefix for the output files, calculated from input if not given"
  , temperature = 0.01  &= help "lower temperatures favor the more optimal paths, defaults to 0.01"
  , fillweight  = FWlog
  , fillstyle   = FSfull
  }

main :: IO ()
main = do
  Options{..} <- cmdArgs oOptions
  let filePrefix = maybe (takeBaseName infile) id outprefix
  runHoxCluster fillweight fillstyle temperature infile filePrefix

