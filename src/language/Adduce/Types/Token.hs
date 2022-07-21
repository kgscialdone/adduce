
module Adduce.Types.Token where

import Data.List (intercalate)
import Data.Interned (unintern)
import Data.Interned.String (InternedString)

type Statement = [Token]

data Token = Form String [Token]
           | Ident InternedString
           | NSIdent [InternedString]
           | Block [Statement]
           | IntLit Integer
           | FltLit Double
           | StrLit String
           | StmtEnd
           | Invalid String

instance Show Token where
  show (Form s ts) = intercalate " " $ s : (map show ts)
  show (Ident s)   = unintern s
  show (NSIdent s) = intercalate "->" $ map unintern s
  show (Block ss)  = "(" ++ intercalate ". " (map (intercalate " " . map show) ss) ++ ")"
  show (IntLit x)  = show x
  show (FltLit x)  = show x
  show (StrLit x)  = show x
  show StmtEnd     = "."
  show (Invalid s) = "{Syntax error: " ++ s ++ "}"

