module Simple4.ParsecParser where
import Simple4.Syntax
import Data.Functor
import Control.Monad
import Control.Monad.Error

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as PT
import Text.ParserCombinators.Parsec.Language(emptyDef)

langDef = emptyDef { 
  PT.reservedNames = ["let", "in", "new", "None",
                      "if","then","else"]}
lexer = PT.makeTokenParser langDef

identifier = PT.identifier lexer
integer = PT.integer lexer
symbol = PT.symbol lexer
parens  = PT.parens lexer
reserved = PT.reserved lexer
kw = reserved

runParser :: String -> String -> Either ParseError Defs
runParser info input = parse pProg info input

pProg :: Parser Defs
pProg = pDefs

pDefs :: Parser Defs
pDefs = pDef `sepBy1` pOptionalSemi

pOptionalSemi :: Parser ()
pOptionalSemi = optional $ symbol ";"

pDef :: Parser Def
pDef = (try pFullDef) <|> ((\e -> ("_",e)) <$> pExp)

pFullDef =do
  v <- identifier
  foo <- symbol "="
  e <- pExp
  return $ (v, e)
  
pExp, pTerm, pF :: Parser Exp
pExp = pIf <|> pNew <|> pLet <|> pArith 

pIf = do
  kw "if"
  e1 <- pExp
  kw "then"
  e2 <- pExp
  kw "else"
  e3 <- pExp
  return $ EIf e1 e2 e3
  
pLet :: Parser Exp
pLet = do
  kw "let" 
  ds <- pDefs 
  kw "in"
  e <- pExp
  return $ ELets ds e

pNew = ENew <$> (kw "new" >> pExp)

pArith :: Parser Exp
pArith = pTerm `chainl1` pAdd

pAdd = symbol "+" >> return EAdd

pTerm = pF
pF = EInt <$> integer <|> pIdExp <|> parens pExp 
          <|> EDeref <$> (EVar <$> (symbol "*" >> identifier))
          <|> pRec <|> (kw "None" >> return ENone)
pRec = symbol "{" >> symbol "}" >> return ERecEmpty

{- I = id I'
I' = "." id I'' | eps
I'' = "=" E | eps
-}
pIdExp = identifier >>= pIdExp'
pIdExp' :: Name -> Parser Exp
pIdExp' n = (symbol "." >> identifier >>= pIdExp'' n )
          <|> (return $ EVar n)
          
pIdExp'' n n2 = (symbol "=" >> (ESet n n2 <$> pExp))
                <|> (return $ EGet n n2)