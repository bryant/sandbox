{-# LANGUAGE NoMonomorphismRestriction #-}

import qualified Text.Parsec as P
import Data.Char (isSpace)
import Data.Maybe (catMaybes)
import Control.Applicative ((<$>), (<*>))

many_n n p
    | n <= 0 = return []
    | otherwise = (++) <$> P.count n p <*> P.many p

many1_till p end = (:) <$> p <*> P.manyTill p end

sepby_n 0 p sep = P.sepBy p sep
sepby_n n p sep = do
    xs <- (:) <$> p <*> P.count n (sep >> p)
    xss <- P.many $ sep >> p
    return $ xs ++ xss

try_choices = P.choice . map P.try
look_from = P.lookAhead . try_choices

multi_char_punct = try_choices [hyphen, ellipsis, expando_ellipsis]
    where
    hyphen = many_n 2 $ P.char '-'
    ellipsis = many_n 2 $ P.char '.'
    expando_ellipsis = P.char '.' `sepby2` P.space
    sepby2 = sepby_n 2

nonspace = P.satisfy (not . isSpace)
comma = P.char ','

find_all p = fmap catMaybes $ P.many p_or_not
    where p_or_not = fmap Just (P.try p) P.<|> (P.anyChar >> return Nothing)

word_tokenizer = find_all words
    where
    words = try_choices [multi_char_punct, word, fmap return nonspace]
    word = P.lookAhead word_starter >> nonspace `many1_till` a_wordend
    a_wordend = look_from $ (comma >> look_from wordend') : wordend'
    wordend' = [P.eof, P.space >> return (), nonword_char >> return (),
        multi_char_punct >> return ()]
    word_starter = P.noneOf "\"`:;&#*@-,(){}[]"
    nonword_char = P.oneOf "?!\"';*:@({[]})"
