import Data.List (takeWhile, dropWhile, sortBy)
import Data.Char (isDigit)

acmp :: String -> String -> Ordering
acmp [] [] = EQ
acmp [] (_:_) = LT
acmp (_:_) [] = GT
acmp xall@(x:xs) yall@(y:ys) = case (take_dig xall, take_dig yall) of
    ("", "") -> case compare x y of
        EQ -> acmp xs ys
        ineqord -> ineqord
    ("", _) -> GT
    (_, "") -> LT
    (xn, yn) -> case compare (read_int xn) (read_int yn) of
        EQ -> acmp (drop_dig xall) (drop_dig yall)
        ineqord -> ineqord
    where
    take_dig = takeWhile isDigit
    drop_dig = dropWhile isDigit
    read_int = read :: (Ord a, Read a, Num a) => String -> a

main = putStrLn . unlines $ sortBy acmp names
    where
    names =
        [ "1000X Radonius Maximus"
        , "10X Radonius"
        , "200X Radonius"
        , "20X Radonius"
        , "20X Radonius Prime"
        , "30X Radonius"
        , "40X Radonius"
        , "Allegia 50 Clasteron"
        , "Allegia 500 Clasteron"
        , "Allegia 51 Clasteron"
        , "Allegia 51B Clasteron"
        , "Allegia 52 Clasteron"
        , "Allegia 60 Clasteron"
        , "Alpha 100"
        , "Alpha 2"
        , "Alpha 200"
        , "Alpha 2A"
        , "Alpha 2A-8000"
        , "Alpha 2A-900"
        , "Callisto Morphamax"
        , "Callisto Morphamax 500"
        , "Callisto Morphamax 5000"
        , "Callisto Morphamax 600"
        , "Callisto Morphamax 700"
        , "Callisto Morphamax 7000"
        , "Callisto Morphamax 7000 SE"
        , "Callisto Morphamax 7000 SE2"
        , "QRS-60 Intrinsia Machine"
        , "QRS-60F Intrinsia Machine"
        , "QRS-62 Intrinsia Machine"
        , "QRS-62F Intrinsia Machine"
        , "Xiph Xlater 10000"
        , "Xiph Xlater 2000"
        , "Xiph Xlater 300"
        , "Xiph Xlater 40"
        , "Xiph Xlater 5"
        , "Xiph Xlater 50"
        , "Xiph Xlater 500"
        , "Xiph Xlater 5000"
        , "Xiph Xlater 58"
        ]
