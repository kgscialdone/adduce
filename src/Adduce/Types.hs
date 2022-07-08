
-- | Core types and related functions.
module Adduce.Types where

import Control.Exception
import Data.List
import Data.Map as Map (Map, lookup, insert, empty, keys)
import Data.Maybe
import Adduce.Utils

type Statement = [Token]
type State     = ([Value], Env)

-- | Scope construct containing bound variables, parent scope, and error handlers.
data Env = Env
  { scope  :: Map String Value
  , parent :: Maybe Env
  , errorHandler :: Maybe (String -> Env -> IO State)
  }

-- | Lookup a given name in an `Env`.
envLookup :: String -> Env -> Maybe Value
envLookup name env = Map.lookup name (scope env) ?: (parent env >>= envLookup name)

-- | Set a value in an `Env`'s scope.
define :: Env -> String -> Value -> Env
define env name value = Env (Map.insert name value (scope env)) (parent env) (errorHandler env)

-- | Add the given error handler function to an `Env`.
withErrorHandler :: Env -> (String -> Env -> IO State) -> Env
withErrorHandler env h = Env (scope env) (parent env) (Just h)

-- | Remove an `Env`'s error handler if it exists.
withoutErrorHandler :: Env -> Env
withoutErrorHandler env = Env (scope env) (parent env) Nothing

-- | Deeply recur into an `Env`'s parents until a predicate is met.
findParent :: (Env -> Bool) -> Env -> Maybe Env
findParent f e
  | f e       = Just e
  | otherwise = findParent f =<< parent e

-- | Create a new `Env` with the given `Env` as its parent.
extend :: Env -> Env
extend env = Env Map.empty (Just env) Nothing

instance Show Env where
  show Env { scope=s, parent=p, errorHandler=e } =
    "Env (" ++ unwords (Map.keys s) ++ ")\n  " ++ maybe "No error handler" (const "Has error handler") e ++ "\n" ++ (unlines . Data.List.map ("  "++) . lines) (show p)


-- | The smallest semantically meaningful chunk of an Adduce program.
data Token = Form String [Token]
           | Ident String
           | Block [Statement]
           | IntLit Integer
           | FltLit Double
           | StrLit String
           | BoolLit Bool
           | StmtEnd
           | Invalid String
  deriving Show

-- | A processed value which can appear in Adduce's stack or `Env`.
data Value = VInt Integer
           | VFlt Double
           | VStr String
           | VBool Bool
           | VList [Value]
           | VBlock [Statement] Env
           | VErr String
           | VFunc (State -> State)
           | VIOFn (State -> IO State)

instance Show Value where
  show (VInt x)     = show x
  show (VFlt x)     = show x
  show (VStr x)     = x
  show (VBool x)    = show x
  show (VList x)    = prettyPrint (VList x)
  show (VBlock _ _) = "<block>"
  show (VErr x)     = "Error: " ++ x

instance Eq Value where
  VInt x  == VInt y  = x == y
  VInt x  == VFlt y  = fromIntegral x == y
  VFlt x  == VInt y  = x == fromIntegral y
  VFlt x  == VFlt y  = x == y
  VStr x  == VStr y  = x == y
  VBool x == VBool y = x == y
  _       == _       = False


-- | Typeclass for values which can be coerced to a boolean.
class BoolCoerceable a where
  asBool :: a -> Bool

instance BoolCoerceable Value where
  asBool (VInt x)   = x /= 0
  asBool (VFlt x)   = x /= 0 && x == x
  asBool (VStr x)   = x /= ""
  asBool (VBool x)  = x
  asBool (VList []) = False
  asBool _          = True


-- | Pretty-print a `Value` for display in Lists etc.
prettyPrint :: Value -> String
prettyPrint (VStr x)  = show x
prettyPrint (VList x) = "List(" ++ intercalate ", " (map prettyPrint x) ++ ")"
prettyPrint x         = show x

-- | Return the type name of a `Value`, as expected by the @:@ `Form`
typeName :: Value -> String
typeName (VInt _) = "Int"
typeName (VFlt _) = "Float"
typeName (VStr _) = "String"
typeName (VBool _) = "Bool"
typeName (VList _) = "List"
typeName (VBlock _ _) = "Block"
typeName _ = "Invalid type"

-- | Exception type thrown by Adduce's default error handler.
newtype AdduceError = AdduceError String deriving Show
instance Exception AdduceError

