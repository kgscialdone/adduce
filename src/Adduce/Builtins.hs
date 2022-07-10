{-# LANGUAGE LambdaCase #-}

-- | Definitions of functions used in `Adduce.Prelude.defaultState`.
module Adduce.Builtins where

import Adduce.Types
import Adduce.Interpreter

print state@(State { stack = (x:xs) }) = do
  Prelude.print x
  return $ restack xs state
print state =
  return $ push (VErr "Expected at least 1 value") state

iff (x : y : z : xs) = (if asBool x then y else z) : xs
iff xs               = VErr "Expected a bool and 2 values" : xs

doo state@(State { stack = (VBlock ss _ : xs) }) = do
  newState <- extendScope state
  (State { stack = newStack }) <- handleError =<< (interpret ss $ restack xs $ newState)
  return $ restack newStack state
doo state = return $ push (VErr "Expected a block") state

list state@(State { stack = (VBlock ss _ : xs) }) = do
  newState <- extendScope state
  (State { stack = newStack }) <- handleError =<< (interpret ss $ restack [] $ newState)
  case newStack of
    (e@(VErr _) : _) -> return $ push e state
    _                -> return $ restack (VList newStack : xs) state
list state = return $ push (VErr "Expected a block") state

while state@(State { stack = (c@(VBlock cs _) : b@(VBlock bs _) : xs) }) = do
  newState <- extendScope state
  (State { stack = newStack }) <- handleError =<< (interpret cs $ restack xs $ newState)
  case newStack of
    (VBool True : xs) -> do
      newState <- extendScope state
      (State { stack = newStack }) <- handleError =<< (interpret bs $ restack xs $ newState)
      while $ restack (c : b : newStack) state
    (VBool False : xs) -> return $ restack xs state
    (e@(VErr _) : _)   -> return $ push e state
    _                  -> return $ push (VErr "Expected a boolean") state
while state = return $ push (VErr "Expected 2 blocks") state

raise (VStr x : xs) = VErr x : xs
raise xs            = VErr "Expected a string" : xs

catch state@(State { stack = (VBlock ss _ : xs) }) =
  return $ restack xs $ withErrorH (\err stt -> interpret ss $ restack [VStr err] $ withoutErrorH stt) state
catch state = return $ push (VErr "Expected a block") state

eq (x:y:xs) = VBool (x == y) : xs
eq xs       = VErr "Expected 2 values" : xs

and (x:y:xs)
  | asBool x && asBool y = y : xs
  | otherwise            = VBool False : xs
and xs                   = VErr "Expected 2 values" : xs

or (x:y:xs)
  | asBool x  = x : xs
  | asBool y  = y : xs
  | otherwise = VBool False : xs
or xs         = VErr "Expected 2 values" : xs

not (x:xs) = VBool (Prelude.not $ asBool x) : xs
not xs     = VErr "Expected 1 value" : xs

add (VInt x : VInt y : xs) = VInt (x + y) : xs
add (VInt x : VFlt y : xs) = VFlt (fromIntegral x + y) : xs
add (VFlt x : VInt y : xs) = VFlt (x + fromIntegral y) : xs
add (VFlt x : VFlt y : xs) = VFlt (x + y) : xs
add xs                     = VErr "Expected 2 numbers" : xs

sub (VInt x : VInt y : xs) = VInt (x - y) : xs
sub (VInt x : VFlt y : xs) = VFlt (fromIntegral x - y) : xs
sub (VFlt x : VInt y : xs) = VFlt (x - fromIntegral y) : xs
sub (VFlt x : VFlt y : xs) = VFlt (x - y) : xs
sub xs                     = VErr "Expected 2 numbers" : xs

mul (VInt x : VInt y : xs) = VInt (x * y) : xs
mul (VInt x : VFlt y : xs) = VFlt (fromIntegral x * y) : xs
mul (VFlt x : VInt y : xs) = VFlt (x * fromIntegral y) : xs
mul (VFlt x : VFlt y : xs) = VFlt (x * y) : xs
mul xs                     = VErr "Expected 2 numbers" : xs

div (VInt x : VInt 0 : xs) = VBool False : xs
div (VInt x : VFlt 0 : xs) = VBool False : xs
div (VInt x : VInt y : xs) = VFlt (fromIntegral x / fromIntegral y) : xs
div (VInt x : VFlt y : xs) = VFlt (fromIntegral x / y) : xs
div (VFlt x : VInt y : xs) = VFlt (x / fromIntegral y) : xs
div (VFlt x : VFlt y : xs) = VFlt (x / y) : xs
div xs                     = VErr "Expected 2 numbers" : xs

mod (VInt x : VInt 0 : xs) = VBool False : xs
mod (VInt x : VInt y : xs) = VInt (Prelude.mod x y) : xs
mod xs                     = VErr "Expected 2 numbers" : xs

pow (VInt x : VInt y : xs) = VInt (x ^ y) : xs
pow (VInt x : VFlt y : xs) = VFlt (fromIntegral x ** y) : xs
pow (VFlt x : VInt y : xs) = VFlt (x ** fromIntegral y) : xs
pow (VFlt x : VFlt y : xs) = VFlt (x ** y) : xs
pow xs                     = VErr "Expected 2 numbers" : xs

le (VInt x : VInt y : xs) = VBool (fromIntegral x <= fromIntegral y) : xs
le (VInt x : VFlt y : xs) = VBool (fromIntegral x <= y) : xs
le (VFlt x : VInt y : xs) = VBool (x <= fromIntegral y) : xs
le (VFlt x : VFlt y : xs) = VBool (x <= y) : xs
le xs                     = VErr "Expected 2 numbers" : xs

length (VList x : xs) = VInt (toInteger $ Prelude.length x) : xs
length xs             = VErr "Expected a list" : xs

get (VInt x : VList y : xs)
  | x >= 0 && Prelude.length y > fromInteger x = y !! fromInteger x : xs
  | otherwise                                  = VErr "Invalid list index" : xs
get xs                                         = VErr "Expected an int and a list" :xs

head (VList x : xs)
  | null x    = VErr "Expected a non-empty list" : xs
  | otherwise = Prelude.head x : xs
head (VStr x : xs)
  | null x    = VErr "Expected a non-empty string" : xs
  | otherwise = VStr (show $ Prelude.head x) : xs
head xs       = VErr "Expected a list or string" : xs

tail (VList x : xs)
  | null x    = VErr "Expected a non-empty list" : xs
  | otherwise = VList (Prelude.tail x) : xs
tail (VStr x : xs)
  | null x    = VErr "Expected a non-empty string" : xs
  | otherwise = VStr (Prelude.tail x) : xs
tail xs       = VErr "Expected a list or string" : xs

concat (VList x : VList y : xs) = VList (x ++ y) : xs
concat (VStr  x : VStr  y : xs) = VStr  (x ++ y) : xs
concat xs                       = VErr "Expected 2 lists or 2 strings" : xs

toString (x : xs) = VStr (show x) : xs
toString xs       = VErr "Expected 1 value" : xs

