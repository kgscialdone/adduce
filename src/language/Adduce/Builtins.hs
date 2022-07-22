{-# LANGUAGE LambdaCase #-}

-- | Definitions of functions used in `Adduce.Prelude.defaultState`.
module Adduce.Builtins where

import Control.Monad (foldM)
import Data.Tuple (swap)
import Data.Interned (unintern)

import Adduce.Types
import Adduce.Interpreter

lett :: Macro
lett (Ident x : xs) state
  | x `elem` reservedNames = return (raiseError ("Cannot redefine reserved name `" ++ unintern x ++ "`") state, xs)
  | otherwise              = return (state, (Thunk $ VIOFn lett') : xs)
  where
    lett' state@State { stack = (y : ys) } = return $ restack ys $ setBinding x y state
    lett' state                            = return $ raiseError "Expected 1 value" state
lett xs state = return (raiseError "`Let` must be followed by a valid non-namespaced identifier" state, xs)

deff :: Macro
deff (Ident x : xs) state
  | x `elem` reservedNames = return (raiseError ("Cannot redefine reserved name `" ++ unintern x ++ "`") state, xs)
  | otherwise              = return (state, (Thunk $ VIOFn deff') : xs)
  where
    deff' state@State { stack = (b@(VBlock _ _) : ys) } = return $ restack ys $ setBinding x (VIOFn (\s -> interpretBlock b s)) state
    deff' state                                         = return $ raiseError "Expected a block" state
deff xs state = return (raiseError "`Def` must be followed by a valid non-namespaced identifier" state, xs)

alias :: Macro
alias (Ident x : xs) state
  | x `elem` reservedNames = return (raiseError ("Cannot redefine reserved name `" ++ unintern x ++ "`") state, xs)
  | otherwise              = alias' xs
  where
    alias' (Ident y : xs) = return $ swap (xs, case findDesuffixed y state of
      Just y  -> setBinding x (VAlias y) state
      Nothing -> raiseError ("Unknown symbol `" ++ unintern y ++ "`") state)
    alias' (y@(NSIdent _) : xs) = return $ swap (xs, case findNamespaced y state of
      Right y -> setBinding x (VAlias y) state
      Left e  -> raiseError e state)
alias xs state = return (raiseError "`Alias` must be followed by 2 valid identifiers, the first of which must be non-namespaced" state, xs)

namespace :: Macro
namespace (Ident x : xs) state
  | x `elem` reservedNames = return (raiseError ("Cannot redefine reserved name `" ++ unintern x ++ "`") state, xs)
  | otherwise              = return (state, (Thunk $ VIOFn namespace') : xs)
  where
    namespace' state@State { stack = (b@(VBlock ss be) : ys) } = do
      childState  <- extendScope $ restack [] state
      resultState <- interpret ss childState
      return $ restack ys $ setBinding x (VScope $ scopeId childState) resultState { scopeId = scopeId state }
    namespace' state = return $ raiseError "Expected a block" state
namespace xs state = return (raiseError "`Namespace` must be followed by a valid non-namespaced identifier" state, xs)

print state@(State { stack = (x:xs) }) = do
  case x of
    VStr s -> putStrLn s
    _      -> Prelude.print x
  return $ restack xs state
print state =
  return $ raiseError "Expected at least 1 value" state

iff (x : y : z : xs) = Right $ (if asBool x then y else z) : xs
iff xs               = Left "Expected a bool and 2 values"

doo state@(State { stack = (b@(VBlock _ _) : xs) }) = interpretBlock b $ restack xs state
doo state                                           = return $ raiseError "Expected a block" state

list state@(State { stack = (b@(VBlock _ _) : xs) }) = do
  ns <- interpretBlock b $ restack [] state
  case raised ns of
    Just e  -> return $ raiseError e state
    Nothing -> return $ restack (VList (stack ns) : xs) state
list state = return $ raiseError "Expected a block" state

loop state@(State { stack = (b@(VBlock _ _) : xs) }) = loop' $ restack xs state where
  loop' st = do
    ns <- interpretBlock b st
    case raised ns of
      Just "$_adduce_internal__BREAK_" -> return $ restack (stack ns) st
      Just e                           -> return $ raiseError e $ restack xs st
      Nothing                          -> loop' $ restack (stack ns) st
loop state = return $ raiseError "Expected a block" state

mapI state@(State { stack = (b@(VBlock _ _) : VList l : xs) }) = do
  nl <- foldM (\s (i,x) -> interpretBlock b $ restack (x : VInt i : stack s) s) (restack [] state) $ zip (map toInteger [0..length l]) l
  return $ restack (VList (reverse $ stack nl) : xs) state

raise state@(State { stack = (VStr x : xs) }) = return $ restack xs $ raiseError x state
raise state                                   = return $ raiseError "Expected a string" state

catch state@(State { stack = (b@(VBlock _ _) : xs) }) =
  return $ restack xs $ withErrorH (\err stt -> interpretBlock b $ restack [VStr err] $ withoutErrorH stt { raised = Nothing }) state
catch state = return $ raiseError "Expected a block" state

eq (x:y:xs) = Right $ VBool (x == y) : xs
eq xs       = Left "Expected 2 values"

and (x:y:xs)
  | asBool x  = Right $ y : xs
  | otherwise = Right $ x : xs
and xs        = Left "Expected 2 values"

or (x:y:xs)
  | asBool x  = Right $ x : xs
  | otherwise = Right $ y : xs
or xs         = Left "Expected 2 values"

not (x:xs) = Right $ VBool (Prelude.not $ asBool x) : xs
not xs     = Left "Expected 1 value"

add (VInt x : VInt y : xs) = Right $ VInt (x + y) : xs
add (VInt x : VFlt y : xs) = Right $ VFlt (fromIntegral x + y) : xs
add (VFlt x : VInt y : xs) = Right $ VFlt (x + fromIntegral y) : xs
add (VFlt x : VFlt y : xs) = Right $ VFlt (x + y) : xs
add xs                     = Left "Expected 2 numbers"

sub (VInt x : VInt y : xs) = Right $ VInt (x - y) : xs
sub (VInt x : VFlt y : xs) = Right $ VFlt (fromIntegral x - y) : xs
sub (VFlt x : VInt y : xs) = Right $ VFlt (x - fromIntegral y) : xs
sub (VFlt x : VFlt y : xs) = Right $ VFlt (x - y) : xs
sub xs                     = Left "Expected 2 numbers"

mul (VInt x : VInt y : xs) = Right $ VInt (x * y) : xs
mul (VInt x : VFlt y : xs) = Right $ VFlt (fromIntegral x * y) : xs
mul (VFlt x : VInt y : xs) = Right $ VFlt (x * fromIntegral y) : xs
mul (VFlt x : VFlt y : xs) = Right $ VFlt (x * y) : xs
mul xs                     = Left "Expected 2 numbers"

div (VInt x : VInt 0 : xs) = Right $ VFlt 0 : xs
div (VInt x : VFlt 0 : xs) = Right $ VFlt 0 : xs
div (VInt x : VInt y : xs) = Right $ VFlt (fromIntegral x / fromIntegral y) : xs
div (VInt x : VFlt y : xs) = Right $ VFlt (fromIntegral x / y) : xs
div (VFlt x : VInt y : xs) = Right $ VFlt (x / fromIntegral y) : xs
div (VFlt x : VFlt y : xs) = Right $ VFlt (x / y) : xs
div xs                     = Left "Expected 2 numbers"

mod (VInt x : VInt 0 : xs) = Right $ VInt 0 : xs
mod (VInt x : VInt y : xs) = Right $ VInt (Prelude.mod x y) : xs
mod xs                     = Left "Expected 2 numbers"

pow (VInt x : VInt y : xs) = Right $ VInt (x ^ y) : xs
pow (VInt x : VFlt y : xs) = Right $ VFlt (fromIntegral x ** y) : xs
pow (VFlt x : VInt y : xs) = Right $ VFlt (x ** fromIntegral y) : xs
pow (VFlt x : VFlt y : xs) = Right $ VFlt (x ** y) : xs
pow xs                     = Left "Expected 2 numbers"

le (VInt x : VInt y : xs) = Right $ VBool (fromIntegral x <= fromIntegral y) : xs
le (VInt x : VFlt y : xs) = Right $ VBool (fromIntegral x <= y) : xs
le (VFlt x : VInt y : xs) = Right $ VBool (x <= fromIntegral y) : xs
le (VFlt x : VFlt y : xs) = Right $ VBool (x <= y) : xs
le xs                     = Left "Expected 2 numbers"

head (VList x : xs)
  | null x    = Left "Expected a non-empty list"
  | otherwise = Right $ Prelude.head x : xs
head (VStr x : xs)
  | null x    = Left "Expected a non-empty string"
  | otherwise = Right $ VStr [Prelude.head x] : xs
head xs       = Left "Expected a list or string"

tail (VList x : xs)
  | null x    = Left "Expected a non-empty list"
  | otherwise = Right $ VList (Prelude.tail x) : xs
tail (VStr x : xs)
  | null x    = Left "Expected a non-empty string"
  | otherwise = Right $ VStr (Prelude.tail x) : xs
tail xs       = Left "Expected a list or string"

concat (VList x : VList y : xs) = Right $ VList (x ++ y) : xs
concat (VStr  x : VStr  y : xs) = Right $ VStr  (x ++ y) : xs
concat xs                       = Left "Expected 2 lists or 2 strings"

toString s@(VStr x : xs) = Right s
toString (x : xs)        = Right $ VStr (show x) : xs
toString xs              = Left "Expected 1 value"

typeOf (x : xs) = Right $ VStr (typeName x) : xs
typeOf xs       = Left "Expected 1 value"

