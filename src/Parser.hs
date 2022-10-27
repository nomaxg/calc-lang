module Parser
  ( module Parser
  ) where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

binary s f = Ex.Infix (reservedOp s >> return (BinOp f))

table =
  [ [ binary "*" Times Ex.AssocLeft
    , binary "/" Divide Ex.AssocLeft
    , binary "%" Mod Ex.AssocLeft
    ]
  , [binary "+" Plus Ex.AssocLeft, binary "-" Minus Ex.AssocLeft]
  ]

int :: Parser Expr
int = do
  Float . fromInteger <$> integer

floating :: Parser Expr
floating = do
  Float <$> float

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

variable :: Parser Expr
variable = do
  Var <$> identifier

assign :: Parser Expr
assign = do
  name <- identifier
  reserved "="
  Assign name <$> expr

factor :: Parser Expr
factor = try assign <|> try floating <|> variable <|> parens expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel =
  many $ do
    ex <- expr
    reservedOp ";"
    return ex

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel = parse (contents toplevel) "<stdin>"
