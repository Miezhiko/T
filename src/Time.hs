{-# LANGUAGE
    BangPatterns
  , RankNTypes
  , ScopedTypeVariables
  , UnicodeSyntax
  #-}

module Time
  ( diffTime
  , humanReadableTimeDiff
  ) where

import           Data.Time

diffTime ∷ ZonedTime -> IO NominalDiffTime
diffTime c1 = do
  c2 <- getCurrentTime
  pure $ diffUTCTime c2 (zonedTimeToUTC c1)

humanReadableTimeDiff ∷ NominalDiffTime -> String
humanReadableTimeDiff = helper
 where
  minutes ∷ NominalDiffTime -> Double
  minutes n = realToFrac $ n / 60

  hours ∷ NominalDiffTime -> Double
  hours   n = minutes n / 60

  days ∷ NominalDiffTime -> Double
  days    n = hours n / 24

  weeks ∷ NominalDiffTime -> Double
  weeks   n = days n / 7

  i2s ∷ RealFrac α => α -> String
  i2s !n = show m
    where m ∷ Int
          m = truncate n

  i2sm ∷ RealFrac α => α -> String
  i2sm !n = show m
    where m ∷ Int
          m = truncate n `mod` 60

  i2sh ∷ RealFrac α => α -> String
  i2sh !n = show m
    where m ∷ Int
          m = truncate n `mod` 24

  helper !d | d < 1          = "one second"
            | d < 60         = i2s d ++ " seconds"
            | minutes d < 2  = "one minute"
            | minutes d < 60 = i2s (minutes d) ++ " minutes"
            | hours d < 2    = "one hour " ++ i2sm (minutes d) ++ " minutes"
            | hours d < 24   = i2s (hours d) ++ " hours " ++ i2sm (minutes d) ++ " minutes"
            | days d < 10    = i2s (days d)  ++ " days " ++ i2sh (hours d) ++ " hours "
            | weeks d < 2    = i2s (weeks d) ++ " week"
            | weeks d < 5    = i2s (weeks d)  ++ " weeks"
            | otherwise      = "more than year ago"
