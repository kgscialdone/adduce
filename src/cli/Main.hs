-- Adduce Interpreter v0.1.0
-- by Katrina Scialdone

module Main where

import Control.Monad (void)

import System.Environment
import System.Exit

import Adduce.Interpreter
import Adduce.Prelude
import Adduce.Types

version = "0.1.0"

-- | Interpeter program entry point.
--   Takes a path to the desired Adduce program from the command line.

--   TODOOO: Make CLI more flexible (work around flags, etc.)
--   TODOO: Add more CLI options
main = do
  args <- getArgs
  case args of
    ["--version"] -> putStrLn version
    ["-v"]        -> putStrLn version

    ["run", path] -> do
      program <- readFile path
      void $ exec program =<< extendScope =<< loadPrelude

    _ -> do
      putStrLn usage
      exitFailure

-- | Usage string printed when incorrect arguments are given.
usage = unlines [
  "Usage: adduce [flags] <subcommand>\n",
  "Flags:",
  "  --version, -v \tShow the current version",
  "",
  "Subcommands:",
  "  run <path>    \tRun an Adduce program from a .adc file"
  -- "  test      \tRun programs in ./test, checking their output against matching .out files"
  ]

-- | Quick run function for use in GHCI.
run s = exec s =<< extendScope =<< loadPrelude

