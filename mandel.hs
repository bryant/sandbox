import Data.Complex (Complex((:+)), realPart)
import System.Environment (getArgs)

mandel z0 = length . take 80 . takeWhile under2 $ iterate (\z -> z ^ 2 + z0) z0
    where under2 = ((<= 2) . realPart . abs)

main = do
    [width, height, xmax, ymax] <- map read <$> getArgs :: IO [Int]
    let (w, h) = (width `quot` 2, height `quot` 2)
    putStrLn $ unlines
        [   [put . mandel $ xx :+ yy
            | x <- [0..width - 1], let xx = fi xmax * fi x / fi width]
        | y <- [0..height - 1], let yy = fi ymax * fi y / fi height]
    where
    put n
        | 80 <= n= '@'
        | 60 <= n && n < 80 = '#'
        | 40 <= n && n < 60 = '*'
        | 20 <= n && n < 40 = '-'
        | otherwise = '.'

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
