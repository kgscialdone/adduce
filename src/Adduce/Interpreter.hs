{-# LANGUAGE LambdaCase #-}

-- | Core language interpreter
module Adduce.Interpreter where

import Control.Exception
import Control.Monad
import Data.List (reverse)
import Data.Map (keys, lookup, empty)
import Data.Maybe (isJust, fromJust, fromMaybe, mapMaybe)

import Adduce.Types
import Adduce.Parser
import Adduce.Utils

-- | Execute an Adduce program from a `String` of its source code and a parent `Env`.
--   Returns the resulting `Env` after execution, or `Nothing` if an error occurred.
exec :: String -> Env -> IO (Maybe Env)
exec program env = catch (do
  case parseString program of
    Right statements -> do
      (stack, env) <- interpret statements ([], env)
      handleError (stack, fromJust $ findParent (isJust . errorHandler) env)
      return $ Just env
    Left syntaxErrors -> do
      putStrLn $ init $ unlines syntaxErrors
      return Nothing)
  (\(AdduceError e) -> do
    putStrLn $ "Error: " ++ e
    return Nothing)

-- | Execute a parsed Adduce program.
--   May mutually recurse with various functions in `Adduce.Prelude`.
interpret :: [Statement] -> State -> IO State
interpret statements state =
  handleError =<< foldPartialIO (handleError' interpret') state (map reverse statements)
  where
    interpret' :: Statement -> State -> IO State
    interpret' stmt (stack, env) =
      fromEither `fmap` handleError' (\s _ -> interpret'' s) (stack, env) stmt
      where
        interpret'' :: Statement -> IO State
        interpret'' (IntLit x : xs)  = interpret' xs (VInt x : stack,                 env)
        interpret'' (FltLit x : xs)  = interpret' xs (VFlt x : stack,                 env)
        interpret'' (StrLit x : xs)  = interpret' xs (VStr x : stack,                 env)
        interpret'' (BoolLit x : xs) = interpret' xs (VBool x : stack,                env)
        interpret'' (Block ss : xs)  = interpret' xs (VBlock ss (extend env) : stack, env)

        interpret'' (Form "Let" [Ident x] : xs) = let' stack where
          let' (y:ys) = interpret' xs (ys, define env x y)
          let' _      = interpret' xs ([VErr "Expected 1 value"], env)

        interpret'' (Form "Def" [Ident x] : xs) = def' stack where
          def' (VBlock ss be : ys) = interpret' xs (ys, define env x (VIOFn (\(s,e) -> do (ns,_) <- interpret ss (s,be); return (ns,e))))
          def' _                   = interpret' xs ([VErr "Expected a block"], env)

        interpret'' (Form ":" [Ident x] : xs) = tch' x stack where
          tch' "Int"    (VInt y : ys)     = interpret' xs (stack, env)
          tch' "Float"  (VFlt y : ys)     = interpret' xs (stack, env)
          tch' "String" (VStr y : ys)     = interpret' xs (stack, env)
          tch' "Bool"   (VBool y : ys)    = interpret' xs (stack, env)
          tch' "List"   (VList y : ys)    = interpret' xs (stack, env)
          tch' "Block"  (VBlock y z : ys) = interpret' xs (stack, env)
          tch' _ (y:ys) = interpret' xs (VErr ("Type error, expected `"++x++"` but got `"++typeName y++"`") : ys, env)
          tch' _ _      = interpret' xs (VErr "Expected 1 value" : stack, env)

        interpret'' (Form "?:" [Ident x] : xs) = tch' x stack where
          tch' "Int"    (VInt y : ys)     = interpret' xs (VBool True : ys, env)
          tch' "Float"  (VFlt y : ys)     = interpret' xs (VBool True : ys, env)
          tch' "String" (VStr y : ys)     = interpret' xs (VBool True : ys, env)
          tch' "Bool"   (VBool y : ys)    = interpret' xs (VBool True : ys, env)
          tch' "List"   (VList y : ys)    = interpret' xs (VBool True : ys, env)
          tch' "Block"  (VBlock y z : ys) = interpret' xs (VBool True : ys, env)
          tch' _ (_:ys) = interpret' xs (VBool False : ys, env)
          tch' _ _      = interpret' xs (VErr "Expected 1 value" : stack, env)

        interpret'' (Form "Alias" [Ident a, Ident b] : xs) = case envLookup b env of
          Just x  -> interpret' xs (stack, define env a x)
          Nothing -> interpret' xs (VErr ("Unknown symbol `" ++ b ++ "`") : stack, env)

        interpret'' (Ident x : xs) = case envLookup x env of
          Just (VFunc f)  -> interpret' xs $ f (stack,env)
          Just (VIOFn f)  -> interpret' xs =<< f (stack,env)
          Just x          -> interpret' xs (x : stack, env)
          Nothing         -> interpret' xs (VErr ("Unknown symbol `" ++ x ++ "`") : stack, env)

        interpret'' (x:xs) = error $ "Internal error: Unhandled token " ++ show x
        interpret'' []     = return (stack,env)

-- | Attempt to handle a Catchable error.
--   Searches the current `Env` for an error handler, and returns the resulting state.
handleError :: State -> IO State
handleError s = fromEither `fmap` handleError' (\a b -> return b) s []

-- | Attempt to handle a Catchable error.
--   Searches the current `Env` for an error handler, calling a given continuation function
--     if no error is found, and returns an `Either` tagging the resulting state.
--
--   - `Right` represents that the error was properly handled with no re-raise.
--   - `Left` represents that the error wasn't handled, or that the handler raised a new error.
handleError' :: (Statement -> State -> IO State) -> State -> Statement -> IO (Either State State)
handleError' _ (VErr e : xs, env) s = case errorHandler env of
  Nothing -> return $ Left (VErr e : xs, env)
  Just f  -> f e env >>= \case
    (y : ys, _) -> return $ Left (y : xs, withoutErrorHandler env)
    _           -> return $ Right (xs, env)
handleError' c state s = Right `fmap` c s state

