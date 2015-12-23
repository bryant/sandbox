-- uses tseitin to convert an arbitrary boolean expr to cnf

import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as E
import qualified Control.Monad.State as St
import Control.Monad.State (State)

data BoolExp a
    = BinOp BinOp (BoolExp a) (BoolExp a)
    | Not (BoolExp a)
    | Leaf a
    deriving Show

data BinOp = And | Or | Nor | Nand | Xor
    deriving Show

data Tseiterm a = Var a | TNot a
    deriving Show

type PP = P.Parsec String ()

data TseiCtx = TseiCtx { var_map :: [(String, Int)], var_count :: Int }
    deriving Show

str_to_exp :: String -> Either P.ParseError (BoolExp String)
str_to_exp = P.runParser (expr <* P.eof) () ""

expr = E.buildExpressionParser table term
term = parens expr P.<|> var
table = [ [E.Postfix $ lexeme (P.char '\'') *> pure Not]
        , [flip E.Infix E.AssocLeft $ lexeme (P.char '.') *> pure (BinOp And)]
        , [flip E.Infix E.AssocLeft $ lexeme (P.char '+') *> pure (BinOp Or)]
        ]

var :: PP (BoolExp String)
var = Leaf <$> lexeme ident
ident = (:) <$> P.letter <*> P.many P.alphaNum

parens, lexeme :: PP a -> PP a
parens p = P.char '(' *> P.spaces *> p <* P.spaces <* P.char ')'
lexeme p = P.spaces *> p <* P.spaces

tseitin :: Int -> BoolExp String -> State TseiCtx [[Tseiterm Int]]
tseitin this Leaf{} = return []
tseitin this (BinOp op e0 e1) = do
    v0 <- get_var_for e0
    v1 <- get_var_for e1
    left <- tseitin v0 e0
    right <- tseitin v1 e1
    return $ tseitin_binop op this v0 v1 ++ left ++ right

get_var_for (Leaf n) = do
    m <- var_map <$> St.get
    maybe add_new return $ lookup n m
    where add_new = fresh_var >>= \varid -> add_to_map n varid >> return varid
get_var_for _ = fresh_var

add_to_map :: String -> Int -> State TseiCtx ()
add_to_map name varid =
    St.modify $ \c@TseiCtx{var_map=m} -> c { var_map = (name, varid) : m }

fresh_var = do
    c <- St.get
    St.put c { var_count = var_count c + 1 }
    return $ var_count c

tseitin_binop :: BinOp -> t -> t -> t -> [[Tseiterm t]]
tseitin_binop And c a b =
    [[TNot a, TNot b, Var c], [Var a, TNot c], [Var b, TNot c]]
tseitin_binop Nand c a b =
    [[TNot a, TNot b, TNot c], [Var a, Var c], [Var b, Var c]]
tseitin_binop Or c a b =
    [[Var a, Var b, TNot c], [TNot a, Var c], [TNot b, Var c]]
tseitin_binop Nor c a b =
    [[Var a, Var b, Var c], [TNot a, TNot c], [TNot b, TNot c]]
tseitin_binop Xor c a b =
    [[TNot a, TNot b, TNot c], [Var a, Var b, TNot c], [Var a, TNot b, Var c],
     [TNot a, Var b, Var c]]
