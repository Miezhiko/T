{-# LANGUAGE
    RankNTypes
  , UnicodeSyntax
  #-}

module IO
  ( waitForKeyPress
  ) where

import           System.IO

ifReadyDo ∷ Handle → IO α → IO (Maybe α)
ifReadyDo θ χ = hReady θ >>= ζ
   where ζ True = fmap Just χ
         ζ _    = pure Nothing

waitForKeyPress ∷ IO ()
waitForKeyPress = do
  res ← stdin `ifReadyDo` getChar
  case res of
    Just _  → pure ()
    Nothing → waitForKeyPress
