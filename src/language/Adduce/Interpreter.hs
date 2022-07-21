{-# LANGUAGE LambdaCase #-}

-- | Core language interpreter
module Adduce.Interpreter where

import Control.Exception (catch)
import Data.List (isSuffixOf, group)
import Data.Maybe (isJust, fromJust)
import Data.Function ((&))
import Data.Interned (intern, unintern)
import Data.Interned.String (InternedString)

import Adduce.Types.Token
import Adduce.Types.State
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
      fromEither <$> handleError' (\s _ -> interpret'' s) state stmt
      where
        interpret'' :: Statement -> IO State
        interpret'' (IntLit x : xs)  = interpret' xs $ push (VInt x) state
        interpret'' (FltLit x : xs)  = interpret' xs $ push (VFlt x) state
        interpret'' (StrLit x : xs)  = interpret' xs $ push (VStr x) state
        interpret'' (BoolLit x : xs) = interpret' xs $ push (VBool x) state

        interpret'' b@(Block ss : xs)  = if isPure ss
          then interpret' xs $ push (VBlock ss (scopeId state)) state
          else do
            newState <- extendScope state
            interpret' xs $ push (VBlock ss (scopeId newState)) newState { scopeId = scopeId state }
          where
            isPure :: [Statement] -> Bool
            isPure ss = all id $ map (all isPure') ss where
              isPure' (Form "Let" _)       = False
              isPure' (Form "Def" _)       = False
              isPure' (Form "Alias" _)     = False
              isPure' (Form "Namespace" _) = False
              isPure' (Ident s)
                | s `elem` reservedNames   = False
              isPure' _                    = True

        interpret'' (Form "Let" [Ident x] : xs) = let' stk where
          let' (y:ys) = interpret' xs $ setBinding x y $ restack ys state
          let' _      = interpret' xs $ raiseError "Expected 1 value" state

        interpret'' (Form "Def" [Ident x] : xs) = def' stk where
          def' (b@(VBlock _ _) : ys) = interpret' xs $ setBinding x (VIOFn (\s -> interpretBlock b s)) $ restack ys state
          def' _                     = interpret' xs $ raiseError "Expected a block" state

        interpret'' (Form "Alias" [Ident a, Ident b] : xs) = case findDesuffixed b state of
          Just b  -> interpret' xs $ setBinding a (VAlias b) state
          Nothing -> interpret' xs $ raiseError ("Unknown symbol `" ++ unintern b ++ "`") state
        interpret'' (Form "Alias" [Ident a, i@(NSIdent _)] : xs) = case findNamespaced i state of
          Right b -> interpret' xs $ setBinding a (VAlias b) state
          Left e  -> interpret' xs $ raiseError e state

        interpret'' (Form "Namespace" [Ident x] : xs) = ns' stk where
          ns' (b@(VBlock ss be) : ys) = do
            childState  <- extendScope $ restack [] state
            resultState <- interpret ss childState
            interpret' xs $ restack ys $ setBinding x (VScope $ scopeId childState) resultState { scopeId = scopeId state }
          ns' _ = interpret' xs $ raiseError "Expected a block" state

        interpret'' (Ident x : xs)       = interpret' xs =<< resolveIdent where
          resolveIdent = maybe (return $ raiseError ("Unknown symbol `" ++ unintern x ++ "`") state) pushResolved $ getDesuffixed x state
        interpret'' (i@(NSIdent _) : xs) = interpret' xs =<< resolveIdent where
          resolveIdent = either (return . flip raiseError state) pushResolved $ getNamespaced i state

        interpret'' (x:xs) = error $ "Internal error: Unhandled token " ++ show x
        interpret'' []     = return state

        pushResolved (VAlias (s,n)) = let res = getBinding n state { scopeId = s } in
          maybe (return $ raiseError ("Unknown symbol `" ++ unintern n ++ "`") state) pushResolved res
        pushResolved (VIOFn f)      = f state
        pushResolved (VFunc f)      = return $ case f stk of
          Left err -> raiseError err state
          Right ns -> restack ns state
        pushResolved x              = return $ push x state

interpretBlock :: Value -> State -> IO State
interpretBlock (VBlock ss be) state = do
  newState <- interpret ss state { scopeId = be }
  return $ newState { scopeId = scopeId state }


-- | Search a `State` for a given binding, performing desuffixing if needed, by applying the given search function.
resolveDesuffixed :: (InternedString -> State -> Maybe a) -> InternedString -> State -> Maybe a
resolveDesuffixed f name state = f name state ?: resolveDesuffixed' f
  where
    resolveDesuffixed' :: (InternedString -> State -> Maybe a) -> Maybe a
    resolveDesuffixed' f = ["ing", "ion", "ed", "er", "es", "d", "r", "s"] &
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

getDesuffixed  = resolveDesuffixed getBinding
findDesuffixed = resolveDesuffixed findBinding


-- | Resolve a namespaced identifier.
resolveNamespaced :: (InternedString -> State -> Maybe a) -> Token -> State -> Either String a
resolveNamespaced f i@(NSIdent x) state = case foldl resolve (Right $ scopeId state) $ init x of
  Right s -> maybe (Left $ "Unknown symbol `" ++ show i ++ "`") Right $ resolveDesuffixed f (last x) state { scopeId = s }
  Left e  -> Left e
  where
    resolve (Right s) y = case getDesuffixed y state { scopeId = s } of
      Just (VScope s) -> Right s
      Just _          -> Left $ "Attempt to access from non-namespace in `" ++ show i ++ "`"
      Nothing         -> Left $ "Unknown symbol `" ++ show i ++ "`"
    resolve (Left e) _ = Left e

getNamespaced  = resolveNamespaced getBinding
findNamespaced = resolveNamespaced findBinding


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
handleError' c state s = case (raised state, getErrorH state) of
  (Just e, Just eh) -> eh e state >>= \newState -> case raised newState of
    Just e  -> return $ Left $ withoutErrorH $ raiseError e state
    Nothing -> return $ Right $ state { raised = Nothing }
  (Just e, Nothing) -> return $ Left state
  (Nothing, _)      -> Right <$> c s state

