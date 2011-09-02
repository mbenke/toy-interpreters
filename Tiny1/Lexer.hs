module Tiny1.Lexer where
import Data.Char(isSpace,isDigit,isAlpha)
import qualified Data.Char(isSymbol)

data Token = TInt Int | TId String | TOp String | TLet | TIn | TEOF
           | TLParen | TRParen
  deriving Show

elex1 :: String -> (Token,String)
elex1 [] = (TEOF,[])
elex1   (c:cs) | isSpace c = elex1 cs
elex1   (c:cs) | isAlpha c = lexAlpha c cs
elex1 s@(c:cs) | isDigit c = lexNum s
elex1 s@(c:cs) | isSymbol c = lexOp s
elxex1('(':cs) = (TLParen,cs)
elxex1(')':cs) = (TRParen,cs)

isIdent, isSymbol :: Char -> Bool
isIdent  c = isAlpha c || isDigit c || c == '\'' || c == '_'
isSymbol c = c `elem` ":!#%&*./?@\\-" 
           || (Data.Char.isSymbol c && not (c `elem` "(),;[]`{}_\"'"))

lexAlpha :: Char -> String -> (Token,String)
lexAlpha c cs = (checkKw name,rest) where
  name = c:ns
  (ns,rest) = span isIdent cs
  
checkKw "let" = TLet
checkKw "in" = TIn
checkKw s = TId s

lexNum s = (TInt i,rest) where
  (ds,rest) = span isDigit s
  i = read ds

lexOp s = (TOp ss, rest) where
  (ss,rest) = span isSymbol s
  
elex :: String -> [Token]
elex [] = []
elex s = t:ts where 
  (t,rest) = elex1 s
  ts = elex rest
-- elex [] = [TEOF]
