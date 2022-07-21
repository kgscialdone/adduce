
module Adduce.Types.State where

import Control.Exception (Exception)
import Data.Map as Map (Map, insert, lookup, delete, member, keys, elems)
import Data.List (intercalate)
import Data.Unique (Unique, newUnique, hashUnique)
import Data.Interned (unintern)
import Data.Interned.String (InternedString)

import Adduce.Types.Token
import Adduce.Utils

type ErrorHandler = String -> State -> IO State
newtype AdduceError = AdduceError String deriving Show
instance Exception AdduceError

newtype ScopeId = ScopeId Unique deriving (Eq, Ord)
type BindingKey = (ScopeId, InternedString)

instance Show ScopeId where
  show (ScopeId uniq) = show $ hashUnique uniq

data State = State
  { stack    :: [Value]
  , scopeId  :: ScopeId
  , bindings :: Map BindingKey Value
  -- , macros   :: Map BindingKey Macro
  , parents  :: Map ScopeId ScopeId
  , errorhs  :: Map ScopeId (String -> State -> IO State)
  , raised   :: Maybe String
  }

instance Show State where
  show s = intercalate "" [
    "State {\n  stack: ", show (stack s),
    "\n  scopeId: ", show (scopeId s),
    "\n  bindings: ", showMapFull (keyedPartition $ keys (bindings s)),
    "\n  parents: ", showMapComp (parents s),
    "\n  errorhs: ", show (keys (errorhs s)),
    "\n  raised: ", show (raised s),
    "\n}"]
    where
      showMapFull m = "[\n    " ++ intercalate ",\n    " (zipWith (\a b -> show a ++ " => " ++ show b) (keys m) (elems m)) ++ "\n  ]"
      showMapComp m = "[" ++ intercalate "," (zipWith (\a b -> show a ++ "=>" ++ show b) (keys m) (elems m)) ++ "]"

newState :: IO State
newState = do
  newId <- ScopeId <$> newUnique
  return $ State { stack = [], scopeId = newId, bindings = mempty, parents = mempty, errorhs = mempty, raised = Nothing }

push :: Value -> State -> State
push value state = state { stack = value : stack state }

retop :: Value -> State -> State
retop v state = push v state { stack = tail $ stack state }

restack :: [Value] -> State -> State
restack stack state = state { stack = stack }

getBinding :: InternedString -> State -> Maybe Value
getBinding name state = Map.lookup (scopeId state, name) (bindings state) ?: (getBinding name =<< getParent state)

setBinding :: InternedString -> Value -> State -> State
setBinding name value state = state { bindings = Map.insert (scopeId state, name) value (bindings state) }

findBinding :: InternedString -> State -> Maybe BindingKey
findBinding name state | Map.member (scopeId state, name) (bindings state) = Just (scopeId state, name)
findBinding name state = getParent state >>= findBinding name

getParent :: State -> Maybe State
getParent state = Map.lookup (scopeId state) (parents state) >>= \p -> Just state { scopeId = p }

withErrorH :: ErrorHandler -> State -> State
withErrorH eh state = state { errorhs = Map.insert (scopeId state) eh (errorhs state) }

withoutErrorH :: State -> State
withoutErrorH state = state { errorhs = Map.delete (scopeId state) (errorhs state) }

getErrorH :: State -> Maybe ErrorHandler
getErrorH state = Map.lookup (scopeId state) (errorhs state)

raiseError :: String -> State -> State
raiseError err state = state { raised = Just err }

extendScope :: State -> IO State
extendScope state = do
  newId <- ScopeId <$> newUnique
  return $ state { scopeId = newId, parents = Map.insert newId (scopeId state) (parents state) }

findParent :: (State -> Bool) -> State -> Maybe State
findParent f state
  | f state   = Just state
  | otherwise = findParent f =<< getParent state


data Value = VInt Integer
           | VFlt Double
           | VStr String
           | VBool Bool
           | VList [Value]
           | VBlock [Statement] ScopeId
           | VFunc ([Value] -> Either String [Value])
           | VIOFn (State   -> IO State)
           | VAlias BindingKey
           | VScope ScopeId

instance Show Value where
  show (VInt x)       = show x
  show (VFlt x)       = show x
  show (VStr x)       = x
  show (VBool x)      = show x
  show (VList x)      = "List(" ++ intercalate ", " (map prettyPrint x) ++ ")"
  show (VBlock x _)   = "<block " ++ show (Block x) ++ ">"
  show (VFunc _)      = "<func>"
  show (VIOFn _)      = "<iofn>"
  show (VAlias (s,n)) = "<alias " ++ show s ++ ":" ++ unintern n ++ ">"
  show (VScope s)     = "<namespace " ++ show s ++ ">"

instance Eq Value where
  VInt x  == VInt y  = x == y
  VInt x  == VFlt y  = fromIntegral x == y
  VFlt x  == VInt y  = x == fromIntegral y
  VFlt x  == VFlt y  = x == y
  VStr x  == VStr y  = x == y
  VBool x == VBool y = x == y
  _       == _       = False

prettyPrint :: Value -> String
prettyPrint (VStr x)  = show x
prettyPrint x         = show x

asBool :: Value -> Bool
asBool (VInt x)   = x /= 0
asBool (VFlt x)   = x /= 0 && x == x
asBool (VStr x)   = x /= ""
asBool (VBool x)  = x
asBool (VList []) = False
asBool _          = True

typeName :: Value -> String
typeName (VInt _) = "Int"
typeName (VFlt _) = "Float"
typeName (VStr _) = "String"
typeName (VBool _) = "Bool"
typeName (VList _) = "List"
typeName (VBlock _ _) = "Block"
typeName (VScope _) = "Namespace"
typeName _ = "Invalid type"

