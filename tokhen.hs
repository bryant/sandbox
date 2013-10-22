import System.Environment (getArgs)

tokenize [] = []
tokenize (char:rest)
    | char `elem` "1234" = 'n' : tokenize rest
    | otherwise = error $ "Cannot tokenize " ++ [char]

main = do
    k <- getArgs
    if length k == 0 then
        print "Moar args"
    else
        print $ tokenize (head k)
