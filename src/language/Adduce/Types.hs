
module Adduce.Types where

import Data.Map as Map (Map, insert, lookup, delete, member, keys, elems)
import Data.List (intercalate)
import Data.Unique (Unique, newUnique, hashUnique)
import Data.Interned (unintern)
import Data.Interned.String (InternedString)

import Adduce.Utils

type ErrorHandler = String -> State -> IO State

newtype ScopeId = ScopeId Unique deriving (Eq, Ord)
type BindingKey = (ScopeId, InternedString)

instance Show ScopeId where
  show (ScopeId uniq) = show $ hashUnique uniq

type Macro = ([Token] -> State -> IO (State, [Token]))

data State = State
  { stack    :: [Value]
  , scopeId  :: ScopeId
  , bindings :: Map BindingKey Value
  , macros   :: Map BindingKey Macro
  , parents  :: Map ScopeId ScopeId
  , errorhs  :: Map ScopeId (String -> State -> IO State)
  , raised   :: Maybe String
  }

instance Show State where
  show s = intercalate "" [
    "State {\n  stack: ", show (stack s),
    "\n  scopeId: ", show (scopeId s),
    "\n  bindings: ", showMapFull (keyedPartition $ keys (bindings s)),
    "\n  macros: ", showMapFull (keyedPartition $ keys (macros s)),
    "\n  parents: ", showMapCompact (parents s),
    "\n  errorhs: ", show (keys (errorhs s)),
    "\n  raised: ", show (raised s),
    "\n}"]
    where
      showMapFull m    = "[\n    " ++ intercalate ",\n    " (zipWith (\a b -> show a ++ " => " ++ show b) (keys m) (elems m)) ++ "\n  ]"
      showMapCompact m = "[" ++ intercalate "," (zipWith (\a b -> show a ++ "=>" ++ show b) (keys m) (elems m)) ++ "]"

newState :: IO State
newState = do
  newId <- ScopeId <$> newUnique
  return $ State { stack = [], scopeId = newId, bindings = mempty, macros = mempty, parents = mempty, errorhs = mempty, raised = Nothing }

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

getMacro :: InternedString -> State -> Maybe Macro
getMacro name state = Map.lookup (scopeId state, name) (macros state) ?: (getMacro name =<< getParent state)

setMacro :: InternedString -> Macro -> State -> State
setMacro name value state = state { macros = Map.insert (scopeId state, name) value (macros state) }

findMacro :: InternedString -> State -> Maybe BindingKey
findMacro name state | Map.member (scopeId state, name) (macros state) = Just (scopeId state, name)
findMacro name state = getParent state >>= findMacro name

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
           | VIOFn (State -> IO State)
           | VAlias BindingKey
           | VScope ScopeId
           | VToken Token

instance Show Value where
  show (VInt x)       = show x
  show (VFlt x)       = show x
  show (VStr x)       = show x
  show (VBool x)      = show x
  show (VList x)      = "List(" ++ intercalate ", " (map show x) ++ ")"
  show (VBlock x _)   = "<block " ++ show (Block x) ++ ">"
  show (VFunc _)      = "<func>"
  show (VIOFn _)      = "<iofn>"
  show (VAlias (s,n)) = "<alias " ++ show s ++ ":" ++ unintern n ++ ">"
  show (VScope s)     = "<namespace " ++ show s ++ ">"
  show (VToken t)     = "<token " ++ show t ++ ">"

instance Eq Value where
  VInt x  == VInt y  = x == y
  VInt x  == VFlt y  = fromIntegral x == y
  VFlt x  == VInt y  = x == fromIntegral y
  VFlt x  == VFlt y  = x == y
  VStr x  == VStr y  = x == y
  VBool x == VBool y = x == y
  _       == _       = False

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


type Statement = [Token]

data Token = Ident InternedString
           | NSIdent [InternedString]
           | Block [Statement]
           | IntLit Integer
           | FltLit Double
           | StrLit String
           | StmtEnd
           | Invalid String
           | Thunk String Value

instance Show Token where
  show (Ident s)   = unintern s
  show (NSIdent s) = intercalate "->" $ map unintern s
  show (Block ss)  = "(" ++ intercalate ". " (map (intercalate " " . map show) ss) ++ ")"
  show (IntLit x)  = show x
  show (FltLit x)  = show x
  show (StrLit x)  = show x
  show StmtEnd     = "."
  show (Invalid s) = "{Syntax error: " ++ s ++ "}"
  show (Thunk s x) = "{Macro thunk: " ++ s ++ "}"

