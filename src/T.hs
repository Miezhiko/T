{-# LANGUAGE
    KindSignatures
  , RankNTypes
  , UnicodeSyntax
  #-}

import           System.Console.GetOpt
import           System.Environment (getArgs)
import           System.Exit

import           Control.Monad.Unicode
import           Data.Kind

import           Version
import           Tracker

main ∷ IO ()
main = do (actions, _, _) <- getOpt RequireOrder options <$> getArgs
          Options { optTrack = tracking
                  } <- foldl (≫=) (pure defaultOptions) actions
          tracking

newtype Options = Options
  { optTrack ∷ IO ()
  }

defaultOptions ∷ Options
defaultOptions = Options {
  optTrack = trackTask 0
  }

options ∷ [OptDescr (Options -> IO Options)]
options = [
  Option "v" ["version"] (NoArg showV) "Display Version",
  Option "h" ["help"]    (NoArg (showHelp options)) "Display Help",
  Option "t" ["track"]   (ReqArg gett "Int") "track task",
  Option "l" ["list"]    (NoArg getL) "list all tasks",
  Option "p" ["pause"]   (ReqArg getp "Int") "pause task",
  Option "P" []          (NoArg getP) "pause all tasks",
  Option "r" ["resume"]  (ReqArg getr "Int") "resume task",
  Option "R" []          (NoArg getR) "resume all tasks",
  Option "f" ["finish"]  (ReqArg getf "Int") "finish task",
  Option "F" []          (NoArg getF) "finish all tasks (in case you've forgot starting)"
  ]

gett ∷ ∀ (μ :: Type -> Type). Monad μ => String -> Options -> μ Options
gett arg ο = pure ο { optTrack = trackTask (read arg) }

getp ∷ ∀ (μ :: Type -> Type). Monad μ => String -> Options -> μ Options
getp arg ο = pure ο { optTrack = pauseTask (read arg) }

getr ∷ ∀ (μ :: Type -> Type). Monad μ => String -> Options -> μ Options
getr arg ο = pure ο { optTrack = resumeTask (read arg) }

getf ∷ ∀ (μ :: Type -> Type). Monad μ => String -> Options -> μ Options
getf arg ο = pure ο { optTrack = finishTask (read arg) }

getL ∷ ∀ τ β. τ -> IO β
getL _ = list >> exitSuccess

getP ∷ ∀ τ β. τ -> IO β
getP _ = pauseAll >> exitSuccess

getR ∷ ∀ τ β. τ -> IO β
getR _ = resumeAll >> exitSuccess

getF ∷ ∀ τ β. τ -> IO β
getF _ = finishAll >> exitSuccess
