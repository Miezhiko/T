{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE UnicodeSyntax #-}

module Tracker
  ( trackTask
  , pauseTask
  , resumeTask
  , finishTask

  , pauseAll
  , resumeAll
  , finishAll

  , list
  ) where

import           Prelude.Unicode

import           Data.List
import           Data.Maybe
import           Data.Time

import           System.Directory
import           System.Environment.Executable (getExecutablePath)
import           System.Exit
import           System.FilePath               (takeDirectory, (</>))
import           System.IO
import           System.Time.Utils             (renderSecs)

import           Control.Applicative
import           Control.Monad

import           IO
import           Time
import           Helper

trackFile ∷ Int → String → IO ()
trackFile η startDate = do
  (_, trackFile) ← getTrack η
  trackYaml ← startTrack trackFile
  let trackStart =
        trackYaml { start = startDate
                  , pause = Nothing
                  , tracked = Nothing
                  }
  yEncode trackFile trackStart

iterateTasks ∷ ((String, String) → IO ()) → IO ()
iterateTasks action = do
  workDir ← getWorkDir
  content ← getDirectoryContents workDir
  let tasks = filter (isPrefixOf "task-") content
      abslt = map (\t → (t, workDir </> t)) tasks
  forM_ abslt action

trackTask ∷ Int → IO ()
trackTask η = do
  c1 ← getZonedTime
  let dateString = show c1
  putStrLn dateString
  if η /= 0
    then do
      putStrLn $ "tracking " ++ show η
      trackFile η dateString
    else do putStrLn "Press any key to stop tracking"
            waitForKeyPress
            putStr "Tracked "
            diff ← diffTime c1
            putStrLn $ humanReadableTimeDiff diff

pauseT ∷ (String, String) → IO ()
pauseT (t,p) = do
  trackYaml ← openTrack p
  case trackYaml of
    Nothing    → exitFailure
    Just yaml  → do
      pauseDate ← getZonedTime
      let startDate = read $ start yaml :: ZonedTime
          currentTracked = fromMaybe "0" (tracked yaml)
          currentTime = read currentTracked :: Int
      difft ← diffTime startDate
      let diff = fromEnum difft :: Int
          total = show $ diff + currentTime
          pauseString = show pauseDate
          trackStart  =
            yaml { pause = Just pauseString
                 , tracked = Just total
                 }
      putStrLn $ t ++ " paused at " ++ pauseString
      yEncode p trackStart

resumeT ∷ (String, String) → IO ()
resumeT (t,p) = do
  trackYaml ← openTrack p
  case trackYaml of
    Nothing    → exitFailure
    Just yaml  → do
      resumeDate ← getZonedTime
      let resumeString = show resumeDate
          trackStart =
            yaml { pause = Nothing
                 , start = resumeString
                 }
      putStrLn $ t ++ " resumed at " ++ resumeString
      yEncode p trackStart

getTotalTracked ∷ Track → IO NominalDiffTime
getTotalTracked cfg = do
  difft ← case pause cfg of
      Just _  → return 0
      Nothing → diffTime c1
  let diffInPicos  = fromEnum difft
      totalTracked =
        toEnum (diffInPicos + trackedTime) :: NominalDiffTime
  return totalTracked
 where startDate ∷ String
       startDate = start cfg
       trackedTime ∷ Int
       trackedTime =
         case tracked cfg of
           Just t  → read t :: Int
           Nothing → 0
       c1 ∷ ZonedTime
       c1 = read startDate :: ZonedTime

finishT ∷ Bool → (String, String) → IO ()
finishT remove (t,p) = do
  trackYaml ← openTrack p
  case trackYaml of
    Nothing    → exitFailure
    Just yaml  → do
      totalTracked ← getTotalTracked yaml
      putStr $ "* " ++ t ++ " : "
      putStrLn $ humanReadableTimeDiff totalTracked
      when remove $ removeFile p

pauseTask ∷ Int → IO ()
pauseTask η = pauseT =<< getTrack η

resumeTask ∷ Int → IO ()
resumeTask η = resumeT =<< getTrack η

finishTask ∷ Int → IO ()
finishTask η = finishT True =<< getTrack η

list ∷ IO ()
list = iterateTasks (finishT False)

pauseAll ∷ IO ()
pauseAll = iterateTasks pauseT

resumeAll ∷ IO ()
resumeAll = iterateTasks resumeT

finishAll ∷ IO ()
finishAll = iterateTasks (finishT True)
