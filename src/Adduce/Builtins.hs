{-# LANGUAGE LambdaCase #-}

-- | Definitions of functions used in `Adduce.Prelude.defaultEnv`.
module Adduce.Builtins where

import Data.List
import Adduce.Types
import Adduce.Interpreter

print (x:xs, env) = do
  Prelude.print x
  return (xs, env)
print state =
  return $ _err state "Expected at least 1 value"

iff (VBool True  : y : z : xs, env) = (y : xs, env)
iff (VBool False : y : z : xs, env) = (z : xs, env)
iff state                           = _err state "Expected a bool and 2 values"

doo (VBlock ss _ : xs, env) = do (s,_) <- interpret ss (xs, extend env); return (s,env)
doo state                   = return $ _err state "Expected a block"

list (VBlock ss _ : xs, env) = interpret ss ([], extend env) >>= handleError >>= \case
  (VErr e : xs, _) -> return (VErr e : xs, env)
  (stack, _)       -> return (VList stack : xs, env)
list state = return $ _err state "Expected a block"

while (VBlock cs _ : VBlock bs _ : xs, env) =
  while' =<< handleError =<< interpret cs (xs, extend env)
  where
    while' (VBool True : xs, _)  = do
      (xs, _) <- handleError =<< interpret bs (xs, extend env)
      while' =<< handleError =<< interpret cs (xs, extend env)
    while' (VBool False : xs, _) = return (xs, env)
    while' (VErr e : xs, _)      = return (VErr e : xs, env)
    while' state                 = return $ _err state "Expected a boolean"

raise (VStr x : xs, env) = _err (xs, env) x
raise state              = _err state "Expected a string"

catch (VBlock ss _ : xs, env) = return (xs, withErrorHandler env (\err env -> do
  (s,_) <- interpret ss ([VStr err], withoutErrorHandler env)
  return (s, env)))
catch state                   = return $ _err state "Expected a block"

eq (x:y:xs, env) = (VBool (x == y) : xs, env)
eq state         = _err state "Expected 2 values"

and (x:y:xs, env)
  | asBool x && asBool y = (y : xs, env)
  | otherwise            = (VBool False : xs, env)
and state         = _err state "Expected 2 values"

or (x:y:xs, env)
  | asBool x  = (x : xs, env)
  | asBool y  = (y : xs, env)
  | otherwise = (VBool False : xs, env)
or state         = _err state "Expected 2 values"

not (x:xs, env) = (VBool (Prelude.not $ asBool x) : xs, env)
not state       = _err state "Expected 1 value"

add (VInt x : VInt y : xs, env) = (VInt (x + y) : xs, env)
add (VInt x : VFlt y : xs, env) = (VFlt (fromIntegral x + y) : xs, env)
add (VFlt x : VInt y : xs, env) = (VFlt (x + fromIntegral y) : xs, env)
add (VFlt x : VFlt y : xs, env) = (VFlt (x + y) : xs, env)
add state                       = _err state "Expected 2 numbers"

sub (VInt x : VInt y : xs, env) = (VInt (x - y) : xs, env)
sub (VInt x : VFlt y : xs, env) = (VFlt (fromIntegral x - y) : xs, env)
sub (VFlt x : VInt y : xs, env) = (VFlt (x - fromIntegral y) : xs, env)
sub (VFlt x : VFlt y : xs, env) = (VFlt (x - y) : xs, env)
sub state                       = _err state "Expected 2 numbers"

mul (VInt x : VInt y : xs, env) = (VInt (x * y) : xs, env)
mul (VInt x : VFlt y : xs, env) = (VFlt (fromIntegral x * y) : xs, env)
mul (VFlt x : VInt y : xs, env) = (VFlt (x * fromIntegral y) : xs, env)
mul (VFlt x : VFlt y : xs, env) = (VFlt (x * y) : xs, env)
mul state                       = _err state "Expected 2 numbers"

div (VInt x : VInt 0 : xs, env) = (VBool False : xs, env)
div (VInt x : VFlt 0 : xs, env) = (VBool False : xs, env)
div (VInt x : VInt y : xs, env) = (VFlt (fromIntegral x / fromIntegral y) : xs, env)
div (VInt x : VFlt y : xs, env) = (VFlt (fromIntegral x / y) : xs, env)
div (VFlt x : VInt y : xs, env) = (VFlt (x / fromIntegral y) : xs, env)
div (VFlt x : VFlt y : xs, env) = (VFlt (x / y) : xs, env)
div state                       = _err state "Expected 2 numbers"

mod (VInt x : VInt 0 : xs, env) = (VBool False : xs, env)
mod (VInt x : VInt y : xs, env) = (VInt (Prelude.mod x y) : xs, env)
mod state                       = _err state "Expected 2 numbers"

pow (VInt x : VInt y : xs, env) = (VInt (x ^ y) : xs, env)
pow (VInt x : VFlt y : xs, env) = (VFlt (fromIntegral x ** y) : xs, env)
pow (VFlt x : VInt y : xs, env) = (VFlt (x ** fromIntegral y) : xs, env)
pow (VFlt x : VFlt y : xs, env) = (VFlt (x ** y) : xs, env)
pow state                       = _err state "Expected 2 numbers"

le (VInt x : VInt y : xs, env) = (VBool (fromIntegral x <= fromIntegral y) : xs, env)
le (VInt x : VFlt y : xs, env) = (VBool (fromIntegral x <= y) : xs, env)
le (VFlt x : VInt y : xs, env) = (VBool (x <= fromIntegral y) : xs, env)
le (VFlt x : VFlt y : xs, env) = (VBool (x <= y) : xs, env)
le state                       = _err state "Expected 2 numbers"

length (VList x : xs, env) = (VInt (toInteger $ Prelude.length x) : xs, env)
length state               = _err state "Expected a list"

get (VInt x : VList y : xs, env)
  | x >= 0 && Prelude.length y > fromInteger x = (y !! fromInteger x : xs, env)
  | otherwise                                  = _err (xs, env) "Invalid list index"
get state                                      = _err state "Expected an int and a list"

head (VList x : xs, env)
  | null x    = _err (xs, env) "Expected a non-empty list"
  | otherwise = (Prelude.head x : xs, env)
head (VStr x : xs, env)
  | null x    = _err (xs, env) "Expected a non-empty string"
  | otherwise = (VStr (show $ Prelude.head x) : xs, env)
head state               = _err state "Expected a list or string"

tail (VList x : xs, env)
  | null x    = _err (xs, env) "Expected a non-empty list"
  | otherwise = (VList (Prelude.tail x) : xs, env)
tail (VStr x : xs, env)
  | null x    = _err (xs, env) "Expected a non-empty string"
  | otherwise = (VStr (Prelude.tail x) : xs, env)
tail state               = _err state "Expected a list or string"

concat (VList x : VList y : xs, env) = (VList (x ++ y) : xs, env)
concat (VStr  x : VStr  y : xs, env) = (VStr  (x ++ y) : xs, env)
concat state                         = _err state "Expected 2 lists or 2 strings"

toString (x : xs, env) = (VStr (show x) : xs, env)
toString state         = _err state "Expected 1 value"


-- | Utility function for raising Catchable errors
_err (xs, env) e = (VErr e : xs, env)

