import Data.List (intercalate)
import System.Environment (getArgs)

main = do
    raw <- readFile =<< head `fmap` getArgs
    putStrLn $ "P2 160 120 256\n" ++ rawToPGM raw
    where
        chunk width [] = []
        chunk width xs = a : chunk width b
            where (a, b) = splitAt width xs
        rawToPGM = unlines . joinCols . (chunk 160) . decimaled
        decimaled = map (show . (read :: String -> Int)) . lines
        joinCols = map $ intercalate " "
