getStuff = find 5 where
    find n
        | n == 0 = return []
        | otherwise = do
            ch <- getChar
            if ch `elem` ['a'..'e'] then do
                tl <- find (n-1)
                return (ch:tl)
            else find n

betterStuff = fmap get5 getContents

get5 n = take 5 $ filter (`elem` ['a'..'e']) n

main = do
    putStrLn "put 5, a-e:"
    ooo <- betterStuff
    putStrLn $ show ooo
