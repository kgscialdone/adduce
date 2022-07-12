
module Main where

import Control.Exception (finally)
import Control.Monad (void, unless)
import Data.List (sort, isSuffixOf, intercalate)

import GHC.IO.Handle
import System.IO
import System.Directory
import System.Exit

import Adduce.Interpreter
import Adduce.Prelude
import Adduce.Types
import Utils

testDir = "src/test/scripts/"
outpDir = "src/test/output/"

main = do
  testFiles <- sort <$> filter (".adc" `isSuffixOf`) <$> getDirectoryContents testDir
  outpFiles <- sort <$> filter (".out" `isSuffixOf`) <$> getDirectoryContents outpDir

  unless (map (dropEnd 3) testFiles == map (dropEnd 3) outpFiles) (do
    putStrLn "Error: Test and output files do not match!"
    exitFailure)

  prelude <- loadPrelude
  results <- mapM (runTest prelude) $ zip testFiles outpFiles

  if and results
    then do
      putStrLn "\nAll tests passed sucessfully"
      exitSuccess
    else do
      putStrLn $ "\nTests failed: " ++ intercalate "," (map fst $ filter (not . snd) $ zip testFiles results)
      exitFailure

  where
    pass = "\x001B[1;32m\x2713\x001B[0m Test passed: "
    fail = "\x001B[1;31m\x2717\x001B[0m Test failed: "

    runTest :: State -> (String, String) -> IO Bool
    runTest prelude (prgPath, outPath) = do
      program <- readFile $ testDir ++ prgPath
      expect  <- readFile $ outpDir ++ outPath
      actual  <- captureOutput $ void $ exec program =<< extendScope prelude
      if expect == actual
        then putStrLn $ pass ++ prgPath
        else putStrLn $ unlines [
          fail ++ prgPath,
          "Expected output:\n" ++ (unlines . map ("  | "++) . lines $ expect),
          "Actual output:\n" ++ (unlines . map ("  | "++) . lines $ actual) ]
      return (expect == actual)

    captureOutput :: IO () -> IO String
    captureOutput f = do
      tmpd <- getTemporaryDirectory
      (tmpf, tmph) <- openTempFile tmpd "haskell_stdout"
      stdout_dup <- hDuplicate stdout
      stderr_dup <- hDuplicate stderr
      finally (do hDuplicateTo tmph stdout
                  hDuplicateTo tmph stderr
                  hClose tmph
                  f)
              (do hDuplicateTo stdout_dup stdout
                  hDuplicateTo stderr_dup stderr)
      str <- readFile tmpf
      removeFile tmpf
      return str

