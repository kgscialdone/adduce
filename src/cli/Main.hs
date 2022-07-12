-- Adduce Interpreter v0.1.0
-- by Katrina Scialdone

{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (void)

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

    ["exec", "-d",      program] -> void $ exec program =<< extendScope =<< debugScope
    ["exec", "--debug", program] -> void $ exec program =<< extendScope =<< debugScope
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
      void $ exec program =<< extendScope =<< debugScope

    debugScope :: IO State
    debugScope = loadPrelude >>= extendScope >>= \s ->
      return $ foldr (\(k,v) s -> setBinding k v s) s bindings
      where
        bindings = [
          ("PrStack", VIOFn (\st@(State { stack = s })      -> do print s; return st)),
          ("PrType",  VIOFn (\st@(State { stack = (x:xs) }) -> do putStrLn . typeName $ x; return st)),
          ("PrState", VIOFn (\st -> do print st; return st))
          ]

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

