
-- | Language builtins and prelude file
module Adduce.Prelude where

import Control.Exception (throw)
import Data.Map as Map (fromList)
import Data.Maybe (fromMaybe)

import Adduce.Types
import Adduce.Interpreter
import qualified Adduce.Builtins as B
import Adduce.Utils

import Paths_adduce

-- | Load the prelude file and return its environment.
loadPrelude = do
  prelude <- readFile =<< getDataFileName "src/prelude.adc"
  preludeEnv <- exec prelude $ extend defaultEnv
  return $ fromMaybe (error "Failed to load prelude") preludeEnv

-- | Default program environment.
--   This is seperate from the external prelude file, which will be loaded into it later.
defaultEnv = Env {
  parent = Nothing,
  errorHandler = Just $ \e _ -> return $ throw $ AdduceError e,
  scope = Map.fromList [
    ("True",  VBool True),
    ("False", VBool False),

    ("Print", VIOFn B.print),
    ("Stack", VIOFn (\s -> do print . fst $ s; return s)),
    ("PrEnv", VIOFn (\s -> do print . snd $ s; return s)),

    ("Do",    VIOFn B.doo),
    ("If",    VFunc B.iff),
    ("List",  VIOFn B.list),
    ("While", VIOFn B.while),

    ("==", VFunc B.eq),
    ("&&", VFunc B.and),
    ("||", VFunc B.or),
    ("!",  VFunc B.not),
    ("<=", VFunc B.le),

    ("+", VFunc B.add),
    ("-", VFunc B.sub),
    ("*", VFunc B.mul),
    ("/", VFunc B.div),
    ("%", VFunc B.mod),
    ("^", VFunc B.pow),

    ("Length",      VFunc B.length),
    ("Get",         VFunc B.get),
    ("Head",        VFunc B.head),
    ("Tail",        VFunc B.tail),
    ("Concatenate", VFunc B.concat),

    ("ToString", VFunc B.toString),

    ("Raise", VFunc B.raise),
    ("Catch", VIOFn B.catch)
    ]
}

