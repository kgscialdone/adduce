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

    ["run", "-d",      path] -> run path True
    ["run", "--debug", path] -> run path True
    ["run", "-d"]            -> do putStrLn usage; exitFailure
    ["run", "--debug"]       -> do putStrLn usage; exitFailure
    ["run",            path] -> run path False
    ["run"]                  -> do putStrLn usage; exitFailure

    ["exec", "-d",      program] -> void $ exec program =<< extendScope =<< debugScope =<< loadPrelude
    ["exec", "--debug", program] -> void $ exec program =<< extendScope =<< debugScope =<< loadPrelude
    ["exec", "-d"]               -> do putStrLn usage; exitFailure
    ["exec", "--debug"]          -> do putStrLn usage; exitFailure
    ["exec",            program] -> void $ exec program =<< extendScope =<< loadPrelude
    ["exec"]                     -> do putStrLn usage; exitFailure

    _ -> do putStrLn usage; exitFailure
  where
    run :: String -> Bool -> IO ()
    run path False = do
      program <- readFile path
      void $ exec program =<< extendScope =<< loadPrelude
    run path True = do
      program <- readFile path
      void $ exec program =<< extendScope =<< debugScope =<< loadPrelude

    debugScope :: State -> IO State
    debugScope prelude = extendScope prelude >>= \s -> return $ foldr (\(k,v) s -> setBinding k v s) s bindings
      where
        bindings = [
          ("PrStack", VIOFn (\st@(State { stack = s })      -> do print s; return st)),
          ("PrType",  VIOFn (\st@(State { stack = (x:xs) }) -> do putStrLn . typeName $ x; return st)),
          ("PrState", VIOFn (\st -> do print st; return st))
          ]

-- | Usage string printed when incorrect arguments are given.
usage = unlines [
  "Usage: adduce <subcommand> [flags] <subcommand args>\n",
  "Flags:",
  "  --version, -v \tShow the current version",
  "  --debug, -d   \tRun with debug output functions enabled",
  "",
  "Subcommands:",
  "  run <path>    \tRun an Adduce program from a .adc file",
  "  exec <program>\tRun an Adduce program from a string"
  -- "  test      \tRun programs in ./test, checking their output against matching .out files"
  ]

-- | Quick run function for use in GHCI.
run s = exec s =<< extendScope =<< loadPrelude

