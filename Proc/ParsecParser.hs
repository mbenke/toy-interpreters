module ParsecParser where
import Data.Functor
import Data.Functor.Identity
import Control.Monad

import Text.Parsec hiding(parse,runParser)
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Token as PT
import Text.Parsec.Language(javaStyle)

import qualified Prelude
import Prelude hiding(Num)

-- Given abstract syntax

type Num = Integer
type Var = String
type Pname = String
type DecV = [(Var,Aexp)]
type DecP = [(Pname,Stm)]

data Aexp = N Num | V Var | Mult Aexp Aexp
          | Add Aexp Aexp | Sub Aexp Aexp
          deriving Show

data Bexp = TRUE | FALSE | Neg Bexp | And Bexp Bexp
          | Le Aexp Aexp | Eq Aexp Aexp
          deriving Show

data Stm = Skip | Ass Var Aexp | Comp Stm Stm
         | If Bexp Stm Stm | While Bexp Stm
         | Block DecV DecP Stm | Call Pname
         deriving Show

-- Lexer definition

langDef = javaStyle { 
  PT.reservedNames = ["skip", "while", "do", "var", "proc", "is", "call",
                      "if","then","else", "begin", "end", "true", "false"],
  PT.commentStart = "/*",
  PT.commentEnd = "*/",
  PT.commentLine = "//",
  PT.nestedComments = True
  }
lexer = PT.makeTokenParser langDef

identifier = PT.identifier lexer
integer = PT.integer lexer
symbol = PT.symbol lexer
parens  = PT.parens lexer
reserved = PT.reserved lexer
whiteSpace = PT.whiteSpace lexer

kw = reserved

type Parser a = ParsecT String () Identity a
-- Running parser

runParser :: String -> String -> Either ParseError Stm
runParser info input = Parsec.parse pProg info input

parse input = case runParser "input" input of
  Left e -> error (show e)
  Right s -> s
-- Parser for statement composition
-- The syntax is ambiguous as to priority and associativity of ";"
-- Assume a statement may be a list of single statements separated by ";"

pProg :: Parser Stm
pProg = do { whiteSpace; s <- pStm; eof; return s }

pStm :: Parser Stm
pStm = pStm1 `chainr1` pComp
pComp = symbol ";" >> return Comp

-- A single statement without semicolons on top level
pStm1 :: Parser Stm
pStm1 = pAss <|> pSkip <|> pIf <|> pWhile <|> pBlock <|> pCall <|> parens pStm

pSkip :: Parser Stm
pSkip = kw "skip" >> return Skip

pIf = do
  kw "if"
  b <- pBexp
  kw "then"
  s1 <- pStm
  kw "else"
  s2 <- pStm
  return $ If b s1 s2

-- Arithmetic expressions
pAexp, pTerm, pF :: Parser Aexp
pAexp = pTerm `chainl1` pAdd
pAdd = pBinOp "+" Add <|> pBinOp "-" Sub

pTerm = pF `chainl1` pBinOp "*" Mult
pF = (N <$> integer) <|> (V <$> identifier) <|> try (parens pAexp)

pBinOp :: String -> (a->b->c) -> Parser (a->b->c)
pBinOp sym con = symbol sym >> return con

-- Boolean expressions
pBexp, pTrue, pFalse, pCompare :: Parser Bexp
pBexp = pBexp1 `chainl1` pAnd
pAnd = symbol "&" >> return And
pBexp1 = Neg <$> (symbol "!" >> pBexp2) <|> pBexp2
pBexp2 = pTrue <|> pFalse <|> pCompare <|> try (parens pBexp)
pTrue = kw "true" >> return TRUE
pFalse = kw "false" >> return FALSE
pCompare = pAexp >>= pCompare'
pCompare' :: Aexp -> Parser Bexp
pCompare' a1 = go "=" Eq <|> go "<=" Le where
  go sym con = do {symbol sym; a2 <- pAexp; return (con a1 a2) }
-- pCompare' a1 = op <*> pure a1 <*> pAexp where
--   op = pBinOp "=" Eq <|> pBinOp "<=" Le

pBlock :: Parser Stm
pBlock = do
  kw "begin"
  vs <- pDecVs
  ps <- pDecPs
  stm <- pStm
  kw "end"
  return (Block vs ps stm) 

pAss = do
  v <- identifier
  symbol ":="
  a <- pAexp
  return (Ass v a)

pDecVs :: Parser DecV
pDecVs = pDecV `endBy` (symbol ";")
pDecV :: Parser (Var,Aexp)
pDecV = do
  kw "var"
  v <- identifier
  symbol ":="
  a <- pAexp
  return (v,a)

pDecPs :: Parser DecP
pDecPs = pDecP `endBy` (symbol ";")
pDecP :: Parser (Pname,Stm)
pDecP = do
  kw "proc"
  n <- identifier
  kw "is"
  s <- pStm1
  return (n,s)

pWhile :: Parser Stm
pWhile = do
  kw "while"
  b <- pBexp
  kw "do"
  s <- pStm
  return (While b s)

pCall :: Parser Stm
pCall = kw "call" >> (Call <$> identifier)

main = do
  print $ parse test1
  print $ parse test2

test1 = "/*fac_loop (p.23)*/\ny:=1;\nwhile !(x=1) do (\n y:=y*x;\n x:=x-1\n)"
test2 = "//fac call (p.55)\n\
\begin\n\
\proc fac is\n\
\begin\n\
\var z:=x;\n\
\if x=1 then skip\n\
\else x:=x-1; call fac; y:=z*y end;\n\
\y:=1; call fac end"

