
-- | Adduce parser
module Adduce.Parser where

import Text.Read (readMaybe)
import Data.Char (isDigit, isLower, isSpace)
import Data.List (groupBy, partition, isInfixOf)
import Data.Maybe (isJust, fromJust, mapMaybe)
import Data.Either (isLeft, fromLeft, fromRight)
import Data.Interned (intern, unintern)

import Adduce.Types
import Adduce.Utils

-- | Parse an Adduce program from a string, returning a list of `Statement`s.
parseString :: String -> Either [String] [Statement]
parseString str =
  case length verified of
    0 -> Right parsed
    _ -> Left verified
  where
    parsed   = lines &. map tokenize &. filter (not . null) &. concat &. parse $ str
    verified = mapMaybe verify &. map ("Syntax error: "++) $ parsed

-- | Tokenize a string into workable chunks for Adduce to use.
tokenize :: String -> [Either Token String]
tokenize = tokenize'
  where
    tokenEnd c = isSpace c || elem c ",.()"

    tokenize' :: String -> [Either Token String]
    tokenize' (';':cs)   = []
    tokenize' ('\"':cs)  = takeQuoted '\"' cs
    tokenize' ('\'':cs)  = takeQuoted '\'' cs
    tokenize' ('-':c:cs)
      | isDigit c = takeNumber "-" (c:cs)
      | otherwise = takeUntil tokenEnd ('-':c:cs)
    tokenize' (c:cs)
      | isSpace c || c == ',' = tokenize' cs
      | isLower c             = skipUntil tokenEnd cs
      | isDigit c             = takeNumber "" (c:cs)
      | c `elem` ".()"        = Right [c] : tokenize' cs
      | otherwise             = takeUntil tokenEnd (c:cs)
    tokenize' "" = []

    takeUntil pred cs = Right (takeWhile (not . pred) cs) : skipUntil pred cs
    skipUntil pred cs = tokenize' $ dropWhile (not . pred) cs

    takeNumber :: String -> String -> [Either Token String]
    takeNumber acc ('.':c:cs)
      | '.' `elem` acc && isDigit c = invalidNumber (acc ++ ['.', c]) cs
      | '.' `elem` acc              = Right acc : tokenize' ('.':c:cs)
      | 'e' `elem` acc              = invalidNumber (acc ++ ['e', c]) cs
      | isDigit c                   = takeNumber (acc ++ ['.',c]) cs
      | otherwise                   = Right acc : tokenize' ('.':c:cs)
    takeNumber acc ('E':'-':c:cs)   = takeNumber acc ('e':'-':c:cs)
    takeNumber acc ('e':'-':c:cs)
      | 'e' `elem` acc              = invalidNumber (acc ++ "e-" ++ [c]) cs
      | isDigit c                   = takeNumber (acc ++ "e-" ++ [c]) cs
      | otherwise                   = invalidNumber (acc ++ "e-") cs
    takeNumber acc ('E':c:cs)       = takeNumber acc ('e':c:cs)
    takeNumber acc ('e':c:cs)
      | 'e' `elem` acc              = invalidNumber (acc ++ ['e', c]) cs
      | isDigit c                   = takeNumber (acc ++ ['e', c]) cs
      | otherwise                   = invalidNumber (acc ++ "e") cs
    takeNumber acc (c:cs)
      | tokenEnd c                  = Right acc : tokenize' (c:cs)
      | isDigit c                   = takeNumber (acc ++ [c]) cs
      | otherwise                   = invalidNumber (acc ++ [c]) cs
    takeNumber acc []               = [Right acc]

    invalidNumber acc cs =
      Left (Invalid $ "Invalid numeric literal `"++trim acc++takeWhile isDigit cs++"`") : skipUntil (not . isDigit) cs

    takeQuoted :: Char -> String -> [Either Token String]
    takeQuoted q cs = case rest of
      "" -> [Left $ Invalid "Unmatched quote"]
      _  -> Right (unescape tok++[q]) : tokenize' (tail rest)
      where
        (tok,rest) = foldl takeQuoted' ([q], "") cs
        takeQuoted' (acc, "") c
          | c == q && null acc         = (acc, [q])
          | c == q && last acc == '\\' = (acc ++ [c], "")
          | c == q                     = (acc, [q])
          | otherwise                  = (acc ++ [c], "")
        takeQuoted' (acc, cs) c = (acc, cs ++ [c])

    unescape ('\\':c:cs)
      | c == 'n'  = '\n' : unescape cs
      | otherwise = c : unescape cs
    unescape (c:cs) = c : unescape cs
    unescape ""     = ""

-- | Parse a tokenized Adduce program into a list of `Statement`s.
parse :: [Either Token String] -> [Statement]
parse = parse' &. groupBy groupStatements &. map (filter filterEnds) &. filter (not . null)
  where
    parse' :: [Either Token String] -> [Token]
    parse' lst = let (err,tok) = partition isLeft lst in
      map (fromLeft $ Invalid "Internal error") err ++
      parse'' (map (fromRight "Internal error") tok)

    parse'' :: [String] -> [Token]
    parse'' (".":ts)       = StmtEnd : parse'' ts
    parse'' (('\'':cs):ts) = StrLit (init cs) : parse'' ts
    parse'' (('\"':cs):ts) = StrLit (init cs) : parse'' ts
    parse'' (('`':cs):ts)  = Block [[Ident (intern cs)]] : parse'' ts

    parse'' (")":ts) = Invalid "Unmatched )" : parse'' ts
    parse'' ("(":ts) = let (inner,rest) = consumeParens ts in case length inner of
      0 -> Invalid "Unmatched (" : parse'' (map snd rest)
      _ -> case last inner of
        (1,")") -> Block (parse $ map (Right . snd) $ init inner) : parse'' (map snd rest)
        _       -> Invalid "Unmatched (" : parse'' (map snd rest)

    parse'' (t:ts)
      | isJust (readMaybe t :: Maybe Integer) = IntLit (read t :: Integer) : parse'' ts
      | isJust (readMaybe t :: Maybe Double)  = FltLit (read t :: Double)  : parse'' ts
      | "->" `isInfixOf` t                    = NSIdent (map intern $ split "->" t) : parse'' ts
      | otherwise                             = Ident (intern t) : parse'' ts
    parse'' []                                = []

    consumeParens ts = span (fst &. (> 0)) $ zip (getNestDepth ts) ts
    getNestDepth = scanl (\acc v -> acc + case v of "(" -> 1; ")" -> -1; _ -> 0) 1

    groupStatements StmtEnd _ = False
    groupStatements _ StmtEnd = False
    groupStatements _ _       = True

    filterEnds StmtEnd = False
    filterEnds _       = True

-- | Run post-parse verification on an Adduce `Statement`.
verify :: Statement -> Maybe String
verify = verify' True
  where
    headOnly = map intern ["Let","Def","Alias","Namespace","Macro"]

    verify' False (Ident x : xs)
      | x `elem` headOnly = Just $ "`" ++ unintern x ++ "` cannot appear in the middle of a statement"
      | otherwise         = verify' False xs

    verify' _ (Block ss : xs) = let res = filter isJust $ map verify ss in case length res of
      0 -> verify' False xs
      _ -> Just $ unlines $ map fromJust res

    verify' _ (Invalid x : xs) = Just x
    verify' _ (x:xs)           = verify' False xs
    verify' _ []               = Nothing

