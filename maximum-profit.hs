import Data.List (tails)
import Test.QuickCheck (
      forAll
    , arbitrary
    , quickCheckWith
    , Args(..)
    , Gen
    , sized
    , choose
    , Property
    , stdArgs
    , collect
    , vectorOf
    )

farthest_pair (x:y:[]) = (y, x)
farthest_pair (x:xs)
    | x - minimum xs > high - low = (minimum xs, x)
    | x - low > high - low = (low, x)
    | otherwise = (low, high)
    where (low, high) = farthest_pair xs
farthest_pair _ = error "insufficient time data"

brute_force xs = maximum $ zipWith diff xs . init . tail $ tails xs
    where diff x ys = x - minimum ys

prop_correct = forAll (listOf2 arbitrary) staff
    where
    staff :: [Int] -> Property
    staff xs = collect len $ o_n_soln == brute_force xs
        where
        (low, high) = farthest_pair xs
        o_n_soln = high - low
        len = length xs

listOf2 :: Gen a -> Gen [a]
listOf2 gen = sized $ \n -> choose (2, max 2 n) >>= flip vectorOf gen

main = quickCheckWith pcqjepeux prop_correct
    where pcqjepeux = stdArgs { maxSuccess = 4096 }
