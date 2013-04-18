module Rec1.Parser where
import Rec1.Syntax

import Text.ParserCombinators.Parsec hiding(runParser)
import qualified Text.ParserCombinators.Parsec.Token as PT
import Text.ParserCombinators.Parsec.Language(emptyDef)

-- Caution, Applicative.(<|>) conflicts with Parsec
import Control.Applicative(pure,(<*>),(<*),(*>),liftA2)  -- hiding((<|>),many)
import Data.Functor

langDef = emptyDef { 
  PT.reservedNames = ["let", "in", "new", "do"
                      ,"if","then","else", "label"
                     ]}

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
pProg = pDefs <* eof

pDefs :: Parser Defs
pDefs = pDef `sepBy1` (symbol ";") --pOptionalSemi
--   pDef `sepBy1` pOptionalSemi
pOptionalSemi :: Parser ()
pOptionalSemi = optional $ symbol ";"

pDef :: Parser Def
pDef = (try pFullDef) <|> ((\e -> ("_",e)) <$> pExp)

pFullDef = liftA2 (,)  (identifier <* symbol "=")  pExp

pExp, pTerm, pF :: Parser Exp
pExp = pIf <|> pNew <|> pLet <|> pTerm

pIf :: Parser Exp
pIf = EIf <$> (kw "if" *> pExp) 
          <*> (kw "then" *> pExp) 
          <*> (kw "else" *> pExp)

pLet :: Parser Exp
pLet = ELets <$> (kw "let" *> pDefs) <*> (kw "in" *> pExp)

pNew :: Parser Exp
pNew =  kw "new" *> pure ENew

pTerm = pF

pF = EInt <$> integer <|> pIdExp <|> parens pExp

pIdExp :: Parser Exp               
pIdExp = (flip ($)) <$> identifier <*> pIdExp'

pIdExp' = pDotted <|> pure EVar

pDotted = (flip ($)) <$> (symbol "." *> identifier ) <*> pIdExp'' 

pIdExp'' :: Parser (Name -> Name -> Exp)
pIdExp'' = (\e x1 x2 -> ESet x1 x2 e) <$>(symbol "=" *>  pExp)
                <|> pure EGet 

test1 = parse pExp "t1" "x"
test2 =  runParser "text2" text2
text2 = "l=new; l.x = 1 ;\
\ let x = l.x in if x then x else 42;\
\ y = 2;\
\ l.x"
