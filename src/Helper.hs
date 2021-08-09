{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE UnicodeSyntax #-}

module Helper
  ( getTrack
  , getWorkDir
  , startTrack
  , openTrack
  , module Track
  ) where

import           Prelude.Unicode

import           System.Directory
import           System.Environment.Executable (getExecutablePath)
import           System.FilePath               (takeDirectory, (</>))
import           System.Info                   (os)

import           Control.Applicative
import           Control.Monad

import           Track

condM ∷ Monad μ ⇒ [(μ Bool, μ α)] → μ α
condM ((test, action) : rest) =
  test >>= \τ → if τ then action
                     else condM rest

getWorkDir ∷ IO FilePath
getWorkDir =
  if | os ∈ ["win32", "mingw32", "cygwin32"] →
        (takeDirectory <$> getExecutablePath)
     | otherwise → getHomeDirectory

getTrack ∷ Int → IO (String, FilePath)
getTrack η = getWorkDir >>= \ω →
                return (τ, ω </> τ)
 where τ ∷ String
       τ = "task-" ++ show η

startTrack ∷ String → IO Track
startTrack trackFile =
  condM [ (doesFileExist trackFile, yDecode trackFile ∷ IO Track)
        , ( return True
          , return Track { tracked = Nothing
                         , start = "0"
                         , pause = Nothing
                         })]

openTrack ∷ String → IO (Maybe Track)
openTrack trackFile =
  condM [ (doesFileExist trackFile
         , Just <$> (yDecode trackFile ∷ IO Track))
         , ( return True
           , return Nothing)]
