{-# LANGUAGE
    RankNTypes
  , UnicodeSyntax
  #-}

module Tracker
  ( finishAll
  , finishTask
  , list
  , pauseAll
  , pauseTask
  , resumeAll
  , resumeTask
  , trackTask
  ) where

import           Data.List
import           Data.Maybe
import           Data.Time
import           Data.Time.Parsers

import           System.Directory
import           System.Exit
import           System.FilePath   ((</>))

import           Control.Monad

import           Text.Parsec

import           Helper
import           IO
import           Time

trackFile ∷ Int → String → IO ()
trackFile η startDate = do
  (_, trackingFile) ← getTrack η
  trackYaml ← startTrack trackingFile
  let trackStart =
        trackYaml { start = startDate
                  , pause = Nothing
                  , tracked = Nothing
                  }
  yEncode trackingFile trackStart

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
pauseT (t, p) = do
  trackYaml ← openTrack p
  case trackYaml of
    Nothing    → exitFailure
    Just yaml  → do
      pauseDate ← getZonedTime
      let startDateParsed = parse zonedTime "" $ start yaml
      case startDateParsed of
        Left e    -> putStrLn $ "Error parsing start time " ++ show e
        Right c1  -> do
          let currentTracked = fromMaybe "0" (tracked yaml)
              currentTime = read currentTracked :: Int
          difft ← diffTime c1
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
getTotalTracked cfg =
   case startParsed of
    Left e    -> do
      putStrLn $ "Error parsing start time " ++ show e
      pure 0
    Right c1  -> do
      difft ← case pause cfg of
          Just _  → pure 0
          Nothing → diffTime c1
      let diffInPicos  = fromEnum difft
          totalTracked =
            toEnum (diffInPicos + trackedTime) :: NominalDiffTime
      pure totalTracked
 where startDate ∷ String
       startDate = start cfg
       trackedTime ∷ Int
       trackedTime =
         case tracked cfg of
           Just t  → read t :: Int
           Nothing → 0
       startParsed ∷ Either ParseError ZonedTime
       startParsed = parse zonedTime "" startDate

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
