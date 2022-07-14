-- Adduce Interpreter v0.1.0
-- by Katrina Scialdone

module Main where

import Control.Monad (void)
import Data.Interned (intern)

import System.Environment
import System.Exit

import Adduce.Interpreter
import Adduce.Prelude
import Adduce.Types

-- | Current interpreter version
version = "0.1.0"

-- | Interpeter program entry point.
main = execCli =<< getArgs

-- | Quick run function for use in GHCI.
run s = execCli ["exec", "-d", s]

-- | Process CLI args and take appropriate action.
execCli :: [String] -> IO ()
execCli = \case
    ["--version"] -> putStrLn version
    ["-v"]        -> putStrLn version

    ["run", "-d",      path] -> run path True
    ["run", "--debug", path] -> run path True
    ["run", "-d"]            -> do putStrLn usage; exitFailure
    ["run", "--debug"]       -> do putStrLn usage; exitFailure
    ["run",            path] -> run path False
    ["run"]                  -> do putStrLn usage; exitFailure

    ["exec", "-d",      program] -> void $ exec program =<< extendScope =<< loadPrelude True
    ["exec", "--debug", program] -> void $ exec program =<< extendScope =<< loadPrelude True
    ["exec", "-d"]               -> do putStrLn usage; exitFailure
    ["exec", "--debug"]          -> do putStrLn usage; exitFailure
    ["exec",            program] -> void $ exec program =<< extendScope =<< loadPrelude False
    ["exec"]                     -> do putStrLn usage; exitFailure

    _ -> do putStrLn usage; exitFailure
  where
    run :: String -> Bool -> IO ()
    run path debug = do
      program <- readFile path
      void $ exec program =<< extendScope =<< loadPrelude debug

    usage = unlines [
      "Usage: adduce <subcommand> [flags] <subcommand args>",
      "",
      "Flags:",
      "  --version, -v \tShow the current version",
      "  --debug, -d   \tRun with debug output functions enabled",
      "",
      "Subcommands:",
      "  run <path>    \tRun an Adduce program from a .adc file",
      "  exec <program>\tRun an Adduce program from a string"
      ]

