module Tiny1.ParsecParser where
import Tiny1.Lexer
import Tiny1.Syntax
import Data.Functor
import Control.Monad
import Control.Monad.Error

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos
runParser :: String -> [Token] -> Either ParseError Stmt
runParser info ts = parse pProg info ts 

type TokParser = GenParser Token ()

tokPos t = initialPos "?" -- FIXME

mytoken :: Token -> TokParser Token
mytoken x = token show  tokPos testTok where
  -- tokPos = undefined  -- FIXME
  testTok t = if x == t then Just t else Nothing

idTok :: TokParser String
idTok = token show tokPos testTok where
  -- tokPos = undefined -- FIXME
  testTok (TId s) = Just s
  testTok _ = Nothing


pProg :: TokParser Stmt 
pProg = SBlock <$> pStmts 

pStmts :: TokParser [Stmt]
pStmts = many1 pStmt

pStmt :: TokParser Stmt
pStmt = do
  v <- idTok
  foo <- mytoken $ TOp "="
  e <- pExp
  return $ v := e
  
{-
 E -> TE'
 E' -> +TE' | eps
 T -> F
 F -> n | id | (E)
-}
pExp, pTerm, pF :: TokParser Exp
pExp = pTerm `chainl1` pAdd

pAdd = mytoken  (TOp "+") >> return EAdd

pTerm = pF
pF = EInt <$> pNum <|> EVar <$> idTok

pNum :: TokParser Integer
pNum = token show tokPos testTok where
  -- tokPos = undefined -- FIXME
  testTok (TInt i) = Just (toInteger i)
  testTok _ = Nothing
