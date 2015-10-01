module FormatString where

--cps

lit :: String -> (String -> b) -> String -> b
lit str cont s = cont $ s ++ str

int :: (String -> b) -> String -> Int -> b
int cont = \s int -> cont $ s ++ show int
