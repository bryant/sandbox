{-# LANGUAGE DeriveDataTypeable #-}
import Control.Applicative hiding (many, (<|>))
import Text.Parsec
import Data.Typeable
type Parser a = Parsec String () a

myparser = many1 letter

main :: IO ()
main = test myparser "myparser" "(my (string to) parse)"

test :: (Typeable a, Show a) => Parser a -> String -> String -> IO ()
test parser description string = do
    putStrLn $ "-- " ++ description ++ " on \"" ++ string ++ "\""
    let res = parse parser description string
    case res of
        Left  err   -> print err
        Right value -> putStrLn $ show value ++ " :: " ++ show (typeOf value)
