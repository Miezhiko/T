{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE UnicodeSyntax #-}

module Version
  ( showMyV
  , showV
  , showHelp
  ) where

import           Text.Printf

import           System.Console.GetOpt
import           System.Exit

import           Data.Version          (showVersion)
import qualified Paths_T               as My

showMyV     ∷ String
showMyV      = showVersion My.version

showV       ∷ ∀ τ β. τ → IO β
showV _      = printf showMyV >> exitSuccess

showHelp    ∷ ∀ τ β α. [OptDescr α] → τ → IO β
showHelp o _ = putStrLn (usageInfo "Usage: t [optional things]" o)
                  >> exitSuccess
