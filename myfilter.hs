filter' _ [] = []
filter' f (a:as) = if f a then a : filter' f as else filter' f as

main = print $ filter' (\x -> x `rem` 2 == 0) [1..23]
