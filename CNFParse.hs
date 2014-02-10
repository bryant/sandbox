{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec (skipWhile)
import Data.Attoparsec.Char8
    ( string, space, char, signed, decimal, endOfLine, endOfInput, skipMany
    , sepBy1, skipSpace, isEndOfLine)
import Control.Applicative (pure, (<|>), (<*>), (<$>))
import Control.Monad (replicateM)

-- cool but unused.
sepByTimes 0 _ _ = return []
sepByTimes n match sep = (:) `fmap` match <*> replicateM (n-1) (sep >> match)

comment_line = do
    char 'c' >> skipWhile (not . isEndOfLine)
    endOfLine  -- inefficient?

header_line = do
    char 'p' >> skipSpace >> string "cnf"
    num_vars <- skipSpace >> decimal
    num_clauses <- skipSpace >> decimal
    endOfLine
    return (num_vars, num_clauses)

clauses = do
    vars <- more `sepBy1` space  -- dimacs oddity
    return vars
    where
    more = (:) <$> term <*> (space >> (endbit <|> more))
    endbit = char '0' >> return []
    term = signed decimal

dimacs = do
    skipMany comment_line
    (nv, nc) <- header_line
    clauseses <- clauses `sepBy1` endOfLine
    skipMany endOfLine >> endOfInput
    return (nv, nc, concat clauseses)
