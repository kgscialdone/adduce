{-# LANGUAGE LambdaCase #-}

-- | Language builtins and prelude file
module Adduce.Prelude where

import Data.Maybe (fromMaybe)
import Data.Interned (intern)

import Adduce.Types
import Adduce.Interpreter
import qualified Adduce.Builtins as B

import Paths_adduce

-- | Load the prelude file and return its environment.
loadPrelude debug = do
  prelude <- readFile =<< getDataFileName "src/language/prelude.adc"
  prelude <- exec prelude =<< extendScope =<< if debug then debugState else defaultState
  return $ fromMaybe (errorWithoutStackTrace "Failed to load prelude") prelude

-- | Default program environment.
--   This is seperate from the external prelude file, which will be loaded into it later.
defaultState = newState >>= \s -> return $
  flip (foldr (\(k,v) s -> setBinding k v s)) (map (\(a,b) -> (intern a, b)) bindings) $
  flip (foldr (\(k,v) s -> setMacro   k v s)) (map (\(a,b) -> (intern a, b)) macros) $ s
  where
    bindings = [
      ("Print", VIOFn B.print),
      ("Do",    VIOFn B.doo),
      ("If",    VFunc B.iff),
      ("List",  VIOFn B.list),

      ("Loop", VIOFn B.loop),
      ("MapI", VIOFn B.mapI),

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

      ("Raise", VIOFn B.raise),
      ("Catch", VIOFn B.catch)
      ]

    macros = [
      ("Let", B.lett),
      ("Def", B.deff),
      ("Alias", B.alias),
      ("Namespace", B.namespace),
      ("Macro", B.macro)
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

