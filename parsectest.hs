import Text.ParserCombinators.Parsec

symbols = "abcdefghijklmnopqrstuvwxyz0123456789"

derp :: GenParser Char k Char
derp = oneOf symbols
