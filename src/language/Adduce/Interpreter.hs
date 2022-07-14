{-# LANGUAGE LambdaCase #-}

-- | Core language interpreter
module Adduce.Interpreter where

import Control.Exception (catch)
import Data.List (reverse, isSuffixOf, group)
import Data.Maybe (isJust, fromJust)
import Data.Function ((&))
import Data.Interned (intern, unintern)
import Data.Interned.String (InternedString)

import Adduce.Types
import Adduce.Parser
import Utils

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
      fromEither <$> handleError' (\s _ -> interpret'' s) state stmt
      where
        interpret'' :: Statement -> IO State
        interpret'' (IntLit x : xs)  = interpret' xs $ push (VInt x) state
        interpret'' (FltLit x : xs)  = interpret' xs $ push (VFlt x) state
        interpret'' (StrLit x : xs)  = interpret' xs $ push (VStr x) state
        interpret'' (BoolLit x : xs) = interpret' xs $ push (VBool x) state
        -- interpret'' (SymLit x : xs)  = interpret' xs $ push (VSym x) state
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

        interpret'' (Form "Alias" [Ident a, Ident b] : xs) = case findDesuffixed b state of
          Just b  -> interpret' xs $ setBinding a (VAlias b) state
          Nothing -> interpret' xs $ push (VErr ("Unknown symbol `" ++ unintern b ++ "`")) state

        interpret'' (Ident x : xs) = resolveIdent x state where
          resolveIdent x rst = case getDesuffixed x rst of
            Just (VAlias (s,n)) -> resolveIdent n rst { scopeId = s }
            Just (VFunc f)      -> interpret' xs $ restack (f stk) state
            Just (VIOFn f)      -> interpret' xs =<< f state
            Just x              -> interpret' xs $ push x state
            Nothing             -> interpret' xs $ push (VErr ("Unknown symbol `" ++ unintern x ++ "`")) state

        interpret'' (x:xs) = error $ "Internal error: Unhandled token " ++ show x
        interpret'' []     = return state

getDesuffixed = doDesuffixed getBinding
findDesuffixed = doDesuffixed findBinding

-- | Search a `State` for a given binding, performing desuffixing if needed, by applying the given search function.
doDesuffixed :: (InternedString -> State -> Maybe a) -> InternedString -> State -> Maybe a
doDesuffixed f name state = f name state ?: doDesuffixed' f
  where
    doDesuffixed' :: (InternedString -> State -> Maybe a) -> Maybe a
    doDesuffixed' f = ["ing", "ion", "ed", "er", "es", "d", "r", "s"] &
      filter (`isSuffixOf` (unintern name)) &.
      map (\s -> dropEnd (length s) (unintern name)) &.
      foldr expandOptions [[],[],[],[]] &. concat &.
      map (flip f state . intern) &.
      foldl (?:) Nothing

    expandOptions name [i,j,k,l] = [
      i ++ [name],
      j ++ if length (last $ group name) >= 2 then [dropEnd 1 name] else [],
      k ++ if last name /= 'e' then [name ++ "e"] else [],
      l ++ if length (last $ group name) >= 2 then [(dropEnd 1 name) ++ "e"] else []]

-- | Attempt to handle a Catchable error.
--   Searches the current `Env` for an error handler, and returns the resulting state.
handleError :: State -> IO State
handleError s = fromEither <$> handleError' (\a b -> return b) s []

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
  (_, _)                 -> Right <$> c s state

