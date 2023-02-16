{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE UnicodeSyntax #-}

import Hake

main ∷ IO ()
main = hake $ do
  "clean | clean the project" ∫
    cabal ["clean"] >> removeDirIfExists buildPath

  tExecutable ♯ do
    cabal ["install", "--only-dependencies", "--overwrite-policy=always"]
    cabal ["configure"]
    cabal ["build"]
    getCabalBuildPath appName >>=
      \p -> copyFile p tExecutable
    cleanCabalLocal

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
    if | os ∈ ["win32", "mingw32", "cygwin32"] -> buildPath </> appName ++ "exe"
       | otherwise -> buildPath </> appName
