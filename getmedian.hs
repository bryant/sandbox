import Control.Monad.State (State, get, put, runState)
-- import Debug.Hood.Observe (observe, runO)

median_filter_depth = 7

insertion_sort xs = foldl insert [] xs
    where
    insert xs j = insert' xs j
    insert' [] j = [j]
    insert' (x:xs) j
        | j > x = x : insert xs j
        | otherwise = j : x : xs

partial_sort xs lo hi = insertion_sort . take (hi-lo) $ drop lo xs

type MedianFilter s a = State s a

get_median xs = do
    unsorted <- get
    let sorted = partial_sort xs 0 $ mid + 1
    put sorted
    return $ sorted !! mid
    where mid = median_filter_depth `quot` 2
