{-# LANGUAGE OverloadedStrings #-}

module CNFParse where

import Data.Attoparsec (skipWhile, parseOnly)
import Data.Attoparsec.Char8
    ( string, space, char, signed, decimal, endOfLine, endOfInput, skipMany
    , sepBy1, skipSpace, isEndOfLine)
import Control.Applicative (pure, (<|>), (<*>), (<$>))
import Control.Monad (replicateM)
import Data.ByteString.Char8 (pack)

-- cool but unused.
sepByTimes 0 _ _ = return []
sepByTimes n match sep = (:) `fmap` match <*> replicateM (n-1) (sep >> match)

comment_line = char 'c' >> skipWhile (not . isEndOfLine) >> endOfLine

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
    more = (:) <$> signed decimal <*> (space >> (stop <|> more))
    stop = char '0' >> return []

dimacs = do
    skipMany comment_line
    (nv, nc) <- header_line
    clauseses <- clauses `sepBy1` endOfLine
    skipMany endOfLine >> endOfInput
    return (nv, nc, concat clauseses)

parse_dimacs str = parseOnly dimacs $ pack str
