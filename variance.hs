mean samples = (sum samples) / (fromIntegral $ length samples)
variance samples = (sumSq centered) / (n - 1)
    where
    n = fromIntegral $ length samples
    centered = map ((mean samples) -) samples
    sumSq [] = 0
    sumSq (x:xs) = x**2 + sumSq xs

ivariance samples =
main = putStrLn . show $ variance [1..23]
