
module Main where

import Data.Version (showVersion)
import System.Console.CmdArgs
import System.FilePath

import BioInf.GeneCluEDO

import Paths_Gene_CluEDO (version)



data Options = Options
  { infile      :: FilePath
  , outprefix   :: Maybe String
  , temperature :: Double
  , fillweight  :: FillWeight
  , fillstyle   :: FillStyle
  }
  deriving (Show,Data,Typeable)

oOptions = Options
  { infile      = def     &= args
  , outprefix   = Nothing &= help "prefix for the output files, calculated from input if not given"
  , temperature = 0.01    &= help "lower temperatures favor the more optimal paths, defaults to 0.01"
  , fillweight  = FWlog   &= help "size of boxes: fwlog (log-scaled), fwlinear (linear scaled), fwfull (always full)"
  , fillstyle   = FSfull  &= help "shading of boxes: fsopacitylog (log-scaled), fsopacitylinear (linear scaled), fsfull (always fully opaque)"
  }

allOptions = oOptions
  &= program "GeneCluEDO"
  &= summary ("GeneCluEDO " ++ showVersion version ++ " (c) Christian Höner zu Siederdissen 2017, choener@bioinf.uni-leipzig.de")
  &= verbosity

main :: IO ()
main = do
  Options{..} <- cmdArgs allOptions
  let filePrefix = maybe (takeBaseName infile) id outprefix
  runGeneCluEDO fillweight fillstyle temperature infile filePrefix

