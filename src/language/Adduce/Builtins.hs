{-# LANGUAGE LambdaCase #-}

-- | Definitions of functions used in `Adduce.Prelude.defaultState`.
module Adduce.Builtins where

import Adduce.Types
import Adduce.Interpreter

print state@(State { stack = (x:xs) }) = do
  Prelude.print x
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

loop state@(State { stack = (b@(VBlock _ _) : xs) }) = do
  ns <- interpretBlock b $ restack xs state
  case raised ns of
    Just e  -> return $ raiseError e $ restack xs state
    Nothing -> loop $ restack (b : stack ns) state
loop state = return $ raiseError "Expected a block" state

raise state@(State { stack = (VStr x : xs) }) = return $ restack xs $ raiseError x state
raise state                                   = return $ raiseError "Expected a string" state

catch state@(State { stack = (b@(VBlock _ _) : xs) }) =
  return $ restack xs $ withErrorH (\err stt -> interpretBlock b $ restack [VStr err] $ withoutErrorH stt { raised = Nothing }) state
catch state = return $ raiseError "Expected a block" state

eq (x:y:xs) = Right $ VBool (x == y) : xs
eq xs       = Left "Expected 2 values"

and (x:y:xs)
  | asBool x && asBool y = Right $ y : xs
  | otherwise            = Right $ VBool False : xs
and xs                   = Left "Expected 2 values"

or (x:y:xs)
  | asBool x  = Right $ x : xs
  | asBool y  = Right $ y : xs
  | otherwise = Right $ VBool False : xs
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

toString (x : xs) = Right $ VStr (show x) : xs
toString xs       = Left "Expected 1 value"

typeOf (x : xs) = Right $ VStr (typeName x) : xs
typeOf xs       = Left "Expected 1 value"

