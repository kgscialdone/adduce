
-- | Core types and related functions.
module Adduce.Types where

import Control.Exception (Exception)
import Data.List (intercalate)
import Data.Map as Map (Map, lookup, insert, empty, keys, elems, delete)
import Data.Unique (Unique, newUnique, hashUnique)

import Adduce.Utils

type Statement    = [Token]
type ErrorHandler = String -> State -> IO State

newtype ScopeId = ScopeId Unique deriving (Eq, Ord)

instance Show ScopeId where
  show (ScopeId uniq) = show $ hashUnique uniq

data State = State
  { stack    :: [Value]
  , scopeId  :: ScopeId
  , bindings :: Map (ScopeId, String) Value
  , parents  :: Map ScopeId ScopeId
  , errorhs  :: Map ScopeId (String -> State -> IO State)
  }

instance Show State where
  show s = intercalate "" [
    "State {\n  stack: ", show (stack s),
    "\n  scopeId: ", show (scopeId s),
    "\n  bindings: ", showMapFull (keyedPartition $ keys (bindings s)),
    "\n  parents: ", showMapComp (parents s),
    "\n  errorhs: ", show (keys (errorhs s)),
    "\n}"]
    where
      showMapFull m = "[\n    " ++ intercalate ",\n    " (zipWith (\a b -> show a ++ " => " ++ show b) (keys m) (elems m)) ++ "\n  ]"
      showMapComp m = "[" ++ intercalate "," (zipWith (\a b -> show a ++ "=>" ++ show b) (keys m) (elems m)) ++ "]"

newState :: IO State
newState = do
  newId <- ScopeId `fmap` newUnique
  return $ State { stack = [], scopeId = newId, bindings = empty, parents = empty, errorhs = empty }

push :: Value -> State -> State
push value state = state { stack = value : stack state }

retop :: Value -> State -> State
retop v state = push v state { stack = tail $ stack state }

restack :: [Value] -> State -> State
restack stack state = state { stack = stack }

getBinding :: String -> State -> Maybe Value
getBinding name state = Map.lookup (scopeId state, name) (bindings state) ?: (getBinding name =<< getParent state)

setBinding :: String -> Value -> State -> State
setBinding name value state = state { bindings = Map.insert (scopeId state, name) value (bindings state) }

getParent :: State -> Maybe State
getParent state = Map.lookup (scopeId state) (parents state) >>= \p -> Just state { scopeId = p }

withErrorH :: ErrorHandler -> State -> State
withErrorH eh state = state { errorhs = Map.insert (scopeId state) eh (errorhs state) }

withoutErrorH :: State -> State
withoutErrorH state = state { errorhs = Map.delete (scopeId state) (errorhs state) }

getErrorH :: State -> Maybe ErrorHandler
getErrorH state = Map.lookup (scopeId state) (errorhs state)

extendScope :: State -> IO State
extendScope state = do
  newId <- ScopeId `fmap` newUnique
  return $ state { scopeId = newId, parents = Map.insert newId (scopeId state) (parents state) }

findParent :: (State -> Bool) -> State -> Maybe State
findParent f state
  | f state   = Just state
  | otherwise = findParent f =<< getParent state


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
           | VBlock [Statement] ScopeId
           | VErr String
           | VFunc ([Value] -> [Value])
           | VIOFn (State   -> IO State)

instance Show Value where
  show (VInt x)     = show x
  show (VFlt x)     = show x
  show (VStr x)     = x
  show (VBool x)    = show x
  show (VList x)    = prettyPrint (VList x)
  show (VBlock _ _) = "<block>"
  show (VErr x)     = "Error: " ++ x
  show (VFunc _)    = "<func>"
  show (VIOFn _)    = "<iofn>"

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

