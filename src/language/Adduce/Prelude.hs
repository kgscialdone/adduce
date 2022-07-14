{-# LANGUAGE LambdaCase #-}

-- | Language builtins and prelude file
module Adduce.Prelude where

import Control.Exception (throw)
import Data.Map as Map (fromList)
import Data.Maybe (fromMaybe)
import Data.Interned (intern)

import Adduce.Types
import Adduce.Interpreter
import qualified Adduce.Builtins as B
import Utils

import Paths_adduce

-- | Load the prelude file and return its environment.
loadPrelude debug = do
  prelude <- readFile =<< getDataFileName "src/language/prelude.adc"
  prelude <- exec prelude =<< extendScope =<< if debug then debugState else defaultState
  return $ fromMaybe (errorWithoutStackTrace "Failed to load prelude") prelude

-- | Default program environment.
--   This is seperate from the external prelude file, which will be loaded into it later.
defaultState = newState >>= \s -> return $
  withErrorH (\e st -> return $ throw $ AdduceError e) $
  foldr (\(k,v) s -> setBinding k v s) s $ map (\(a,b) -> (intern a, b)) bindings
  where
    bindings = [
      ("Print", VIOFn B.print),
      ("Do",    VIOFn B.doo),
      ("If",    VFunc B.iff),
      ("List",  VIOFn B.list),

      ("$_loop", VIOFn B.loop),

      ("==",  VFunc B.eq),
      ("<=",  VFunc B.le),
      ("And", VFunc B.and),
      ("Or",  VFunc B.or),
      ("Not", VFunc B.not),

      ("+", VFunc B.add),
      ("-", VFunc B.sub),
      ("*", VFunc B.mul),
      ("/", VFunc B.div),
      ("%", VFunc B.mod),
      ("^", VFunc B.pow),

      ("Head",        VFunc B.head),
      ("Tail",        VFunc B.tail),
      ("Concatenate", VFunc B.concat),

      ("ToString", VFunc B.toString),
      ("TypeOf", VFunc B.typeOf),

      ("Raise", VFunc B.raise),
      ("Catch", VIOFn B.catch)
      ]

-- | Debug bindings, only available with @--debug@ flag.
debugState = defaultState >>= \s -> return $
  foldr (\(k,v) s -> setBinding k v s) s $ map (\(a,b) -> (intern a, b)) bindings
  where
    bindings = [
      ("PrStack", VIOFn (\st@(State { stack = s }) -> do print s; return st)),
      ("PrState", VIOFn (\st                       -> do print st; return st)),
      ("PrType",  VIOFn (\case
            st@(State { stack = (x:xs) }) -> do putStrLn . typeName $ x; return st
            st                            -> return $ raiseError "Expected 1 value" st))
      ]

