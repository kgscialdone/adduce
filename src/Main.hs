-- Adduce Interpreter v0.1.0
-- by Katrina Scialdone

module Main where

import Control.Exception
import Control.Monad
import Data.List (sort, partition, isSuffixOf, intercalate)
import Data.Map as Map (Map, insert, lookup, empty, elems)
import Data.Maybe (fromMaybe, maybe, fromJust)
import Data.Bifunctor (bimap, first)

import GHC.IO.Handle
import System.IO
import System.Directory
import System.Environment
import System.Exit

import Adduce.Types
import Adduce.Prelude
import Adduce.Interpreter
import Adduce.Utils

-- | Interpeter program entry point.
--   Takes a path to the desired Adduce program from the command line.

--   TODOOO: Make CLI more flexible (work around flags, etc.)
--   TODOO: Add more CLI options
main = do
  args <- getArgs
  case args of
    ["run", path] -> do
      preludeEnv <- loadPrelude
      program <- readFile path
      void $ exec program $ extend preludeEnv

    ["test"] -> do
      result <- test
      if result then exitSuccess
                else exitFailure

    _ -> do
      putStrLn usage
      exitFailure

-- | Usage string printed when incorrect arguments are given.
usage = unlines [
  "Usage: adduce <subcommand>\n",
  "Subcommands:",
  "  run <path>\tRun an Adduce program from a .adc file",
  "  test      \tRun programs in ./test, checking their output against matching .out files"
  ]

-- | Quick run function for use in GHCI.
run s = void $ exec s =<< loadPrelude

-- | Simple test runner.
--   Looks in folder @./tests@ for matching @.adc@ and @.out@ files, running the @.adc@ files
--     and checking their output against the contents of the @.out@ files.
--   Returns `True` if all tests passed.

--   TODOOO: Improve test runner to be more parameterized
--   TODO: In-language test suite?
test = do
  testFiles <- sort `fmap` getDirectoryContents "tests"
  testFiles <- return $ foldl groupFiles Map.empty testFiles
  (tests, unmch) <- return $ bimap (map (bimapBoth fromJust)) (map getUnmatched) $ partition bothJust $ elems testFiles
  unless (null unmch) $ putStrLn $ unlines $ map ("Skipping unmatched test file "++) unmch

  preludeEnv <- loadPrelude
  results <- mapM (runTest preludeEnv) tests

  if and results
    then putStrLn "\nAll tests passed sucessfully"
    else putStrLn $ "\nTests failed: " ++ intercalate "," (map fst $ filter (not . snd) $ zip (map fst tests) results)
  return $ and results
  where
    pass = "\x001B[1;32m\x2713\x001B[0m Test passed: "
    fail = "\x001B[1;31m\x2717\x001B[0m Test failed: "

    runTest :: Env -> (String, String) -> IO Bool
    runTest preludeEnv (prgPath, outPath) = do
      program <- readFile $ "tests/" ++ prgPath
      expect  <- readFile $ "tests/" ++ outPath
      actual  <- captureOutput $ void $ exec program $ extend preludeEnv
      if expect == actual
        then putStrLn $ pass ++ prgPath
        else putStrLn $ unlines [
          fail ++ prgPath,
          "Expected output:\n" ++ (unlines . map ("  | "++) . lines $ expect),
          "Actual output:\n" ++ (unlines . map ("  | "++) . lines $ actual) ]
      return (expect == actual)

    groupFiles acc path
      | ".adc" `isSuffixOf` path = insertFst (take (length path - 4) path) path acc
      | ".out" `isSuffixOf` path = insertSnd (take (length path - 4) path) path acc
      | otherwise                = acc

    getUnmatched (Just x, Nothing) = x
    getUnmatched (Nothing, Just x) = x

    insertFst :: Ord k => k -> a -> Map k (Maybe a, Maybe b) -> Map k (Maybe a, Maybe b)
    insertFst key val map = case Map.lookup key map of
      Just (_, b) -> Map.insert key (Just val, b) map
      Nothing     -> Map.insert key (Just val, Nothing) map

    insertSnd :: Ord k => k -> b -> Map k (Maybe a, Maybe b) -> Map k (Maybe a, Maybe b)
    insertSnd key val map = case Map.lookup key map of
      Just (a, _) -> Map.insert key (a, Just val) map
      Nothing     -> Map.insert key (Nothing, Just val) map

    bothJust :: (Maybe a, Maybe b) -> Bool
    bothJust (Nothing, _) = False
    bothJust (_, Nothing) = False
    bothJust _            = True

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

