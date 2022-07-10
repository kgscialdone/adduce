{-# LANGUAGE LambdaCase #-}

-- | Core language interpreter
module Adduce.Interpreter where

import Control.Exception (catch)
import Data.List (reverse, isSuffixOf, group)
import Data.Maybe (isJust, fromJust)
import Data.Function ((&))

import Adduce.Types
import Adduce.Parser
import Adduce.Utils

-- | Execute an Adduce program from a `String` of its source code and a parent `Env`.
--   Returns the resulting `Env` after execution, or `Nothing` if an error occurred.
exec :: String -> State -> IO (Maybe State)
exec program state = catch (
  case parseString program of
    Right statements -> do
      state <- interpret statements state
      handleError $ fromJust $ findParent (isJust . getErrorH) state
      return $ Just state
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
    interpret' stmt state@(State { stack = stk }) =
      fromEither `fmap` handleError' (\s _ -> interpret'' s) state stmt
      where
        interpret'' :: Statement -> IO State
        interpret'' (IntLit x : xs)  = interpret' xs $ push (VInt x) state
        interpret'' (FltLit x : xs)  = interpret' xs $ push (VFlt x) state
        interpret'' (StrLit x : xs)  = interpret' xs $ push (VStr x) state
        interpret'' (BoolLit x : xs) = interpret' xs $ push (VBool x) state
        interpret'' (Block ss : xs)  = do
          newState <- extendScope state
          interpret' xs $ push (VBlock ss (scopeId newState)) newState { scopeId = scopeId state }

        interpret'' (Form "Let" [Ident x] : xs) = let' stk where
          let' (y:ys) = interpret' xs $ setBinding x y $ restack ys state
          let' _      = interpret' xs $ push (VErr "Expected 1 value") state

        interpret'' (Form "Def" [Ident x] : xs) = def' stk where
          def' (VBlock ss be : ys) = interpret' xs $ setBinding x (VIOFn (\s -> do
            ns <- interpret ss s { scopeId = be }
            return $ restack (stack ns) s)) $ restack ys state
          def' _                   = interpret' xs $ push (VErr "Expected a block") state

        interpret'' (Form ":" [Ident x] : xs) = tch' x stk where
          tch' "Int"    (VInt y : _)     = interpret' xs state
          tch' "Float"  (VFlt y : _)     = interpret' xs state
          tch' "String" (VStr y : _)     = interpret' xs state
          tch' "Bool"   (VBool y : _)    = interpret' xs state
          tch' "List"   (VList y : _)    = interpret' xs state
          tch' "Block"  (VBlock y z : _) = interpret' xs state
          tch' _ (y:_) = interpret' xs $ push (VErr ("Type error, expected `"++x++"` but got `"++typeName y++"`")) state
          tch' _ _     = interpret' xs $ push (VErr "Expected 1 value") state

        interpret'' (Form "?:" [Ident x] : xs) = tch' x stk where
          tch' "Int"    (VInt y : _)     = interpret' xs $ retop (VBool True) state
          tch' "Float"  (VFlt y : _)     = interpret' xs $ retop (VBool True) state
          tch' "String" (VStr y : _)     = interpret' xs $ retop (VBool True) state
          tch' "Bool"   (VBool y : _)    = interpret' xs $ retop (VBool True) state
          tch' "List"   (VList y : _)    = interpret' xs $ retop (VBool True) state
          tch' "Block"  (VBlock y z : _) = interpret' xs $ retop (VBool True) state
          tch' _ (_:_) = interpret' xs $ retop (VBool False) state
          tch' _ _     = interpret' xs $ push (VErr "Expected 1 value") state

        interpret'' (Form "Alias" [Ident a, Ident b] : xs) = case getDesuffixed b state of
          Just x  -> interpret' xs $ setBinding a x state
          Nothing -> interpret' xs $ push (VErr ("Unknown symbol `" ++ b ++ "`")) state

        interpret'' (Ident x : xs) = case getDesuffixed x state of
          Just (VFunc f) -> interpret' xs $ restack (f stk) state
          Just (VIOFn f) -> interpret' xs =<< f state
          Just x         -> interpret' xs $ push x state
          Nothing        -> interpret' xs $ push (VErr ("Unknown symbol `" ++ x ++ "`")) state

        interpret'' (x:xs) = error $ "Internal error: Unhandled token " ++ show x
        interpret'' []     = return state

-- | Search a `State` for a given binding, performing desuffixing if needed.
getDesuffixed :: String -> State -> Maybe Value
getDesuffixed name state = getBinding name state ?: getDesuffixed'
  where
    getDesuffixed' = ["ing", "ion", "ed", "er", "es", "d", "r", "s"] &
      filter (`isSuffixOf` name) &.
      map (\s -> dropEnd (length s) name) &.
      foldr expandOptions [[],[],[],[]] &. concat &.
      map (flip getBinding state) &.
      foldl (?:) Nothing

    expandOptions name [i,j,k,l] = [
      i ++ [name],
      j ++ if length (last $ group name) >= 2 then [dropEnd 1 name] else [],
      k ++ if last name /= 'e' then [name ++ "e"] else [],
      l ++ if length (last $ group name) >= 2 then [(dropEnd 1 name) ++ "e"] else []]

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
handleError' c state s = case (stack state, getErrorH state) of
  (VErr e : xs, Nothing) -> return $ Left state
  (VErr e : xs, Just eh) -> eh e state >>= \newState -> case stack newState of
    (y@(VErr _) : _) -> return $ Left  $ retop y $ withoutErrorH state
    _                -> return $ Right $ restack xs state
  (_, _)                 -> Right `fmap` c s state

