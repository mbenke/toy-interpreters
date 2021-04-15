module Tiny2.ParsecParser where
import Tiny2.Syntax
import Data.Functor
import Control.Monad
-- import Control.Monad.Error

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as PT
import Text.ParserCombinators.Parsec.Language(emptyDef)

lexer = PT.makeTokenParser emptyDef

identifier = PT.identifier lexer
integer = PT.integer lexer
symbol = PT.symbol lexer
parens  = PT.parens lexer

runParser :: String -> String -> Either ParseError Stmt
runParser info input = parse pProg info input

pProg :: Parser Stmt 
pProg = SBlock <$> pStmts 

pStmts :: Parser [Stmt]
pStmts = many1 pStmt

pStmt :: Parser Stmt
pStmt = do
  v <- identifier
  foo <- symbol "="
  e <- pExp
  return $ v := e
  
{-
 E -> TE'
 E' -> +TE' | eps
 T -> F
 F -> n | id | (E)
-}
pExp, pTerm, pF :: Parser Exp
pExp = pTerm `chainl1` pAdd

pAdd = symbol "+" >> return EAdd

pTerm = pF
pF = EInt <$> integer <|> EVar <$> identifier <|> parens pExp
