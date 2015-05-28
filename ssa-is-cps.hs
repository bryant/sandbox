-- "SSA is Functional Programming." Appel

module SSAisCPS where

import qualified Text.Parsec as P
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.Language (javaStyle)
import Control.Applicative ((<$>), (<*>), (<*))

type Name = String

data Expr a  -- param = var type
    = EVar a
    | EInt Integer
    | EBinOp Op (Expr a) (Expr a)
    | EUnOp Op (Expr a)
    deriving Show

type Op = String

data Statement a
    = Assign Name (Expr a)  -- a := b + c
    | While (Expr a) [Statement a]  -- while expr { /* non-empty */ }
    | If (Expr a) [Statement a] [Statement a]  -- if expr {} else {]
    deriving Show

type Program a = [Statement a]

-- parsing crud

lex_ = Tok.makeTokenParser javaStyle { Tok.reservedOpNames = words "+ - * /" }
identifier = Tok.identifier lex_
operator = Tok.operator lex_
parens = Tok.parens lex_
reserved_op = Tok.reservedOp lex_
hexadecimal = Tok.hexadecimal lex_
decimal = Tok.decimal lex_
octal = Tok.octal lex_
lexeme = Tok.lexeme lex_
braces = Tok.braces lex_
reserved = Tok.reserved lex_

expr = Ex.buildExpressionParser ops terms
    where
    ops =
        [ [Ex.Prefix (reserved_op "-" >> return (EUnOp "-"))]

        , [ Ex.Infix (reserved_op "*" >> return (EBinOp "*")) Ex.AssocLeft
          , Ex.Infix (reserved_op "/" >> return (EBinOp "/")) Ex.AssocLeft
          ]

        , [ Ex.Infix (reserved_op "+" >> return (EBinOp "+")) Ex.AssocLeft
          , Ex.Infix (reserved_op "-" >> return (EBinOp "-")) Ex.AssocLeft
          ]
        ]

    terms = try_choice $ map lexeme [parens expr, avar, anint]

try_choice = P.choice . map P.try

anint = EInt <$> try_choice [hexadecimal, decimal, octal]

avar = EVar <$> identifier

assignment = Assign <$> identifier <*> (reserved_op ":=" >> expr)
while =  While <$> (reserved "while" >> expr) <*> block
ifelse = If <$> (reserved "if" >> expr) <*> block <*> (reserved "else" >> block)
statement = try_choice [assignment, while, ifelse]
block = braces $ P.many1 statement

program = Tok.whiteSpace lex_ >> P.many1 statement <* P.eof

parse_prog :: String -> Either P.ParseError (Program Name)
parse_prog = P.parse program ""

-- todo: ssa
