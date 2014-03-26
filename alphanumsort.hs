import Data.List (takeWhile, dropWhile)
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
