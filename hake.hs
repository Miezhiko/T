{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE UnicodeSyntax #-}

import Hake

import Data.String.Utils (rstrip)

main ∷ IO ()
main = hake $ do
  "clean | clean the project" ∫
    cabal ["clean"] >> removeDirIfExists buildPath

  tExecutable ♯ do
    cabal ["install", "--only-dependencies", "--overwrite-policy=always"]
    cabal ["configure"]
    cabal ["build"]
    (exitCode, stdOut, stdErr) <-
        readProcessWithExitCode "cabal" ["list-bin", appName] []
    if exitCode == ExitSuccess 
        then let path = rstrip stdOut
             in copyFile path tExecutable
        else putStrLn stdErr
    -- no idea what is it and why do I need it
    removeIfExists "cabal.project.local"

  "install | install to system" ◉ [tExecutable] ∰
    cabal ["install", "--overwrite-policy=always"]

  "test | build and test" ◉ [tExecutable] ∰
    rawSystem tExecutable ["--version"]
      >>= checkExitCode

 where
  appName ∷ String
  appName = "t"

  buildPath ∷ String
  buildPath = "dist-newstyle"

  tExecutable ∷ String
  tExecutable =
    {- HLINT ignore "Redundant multi-way if" -}
    if | os ∈ ["win32", "mingw32", "cygwin32"] → buildPath </> appName ++ "exe"
       | otherwise → buildPath </> appName
