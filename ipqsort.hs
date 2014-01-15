{-# LANGUAGE BangPatterns #-}

import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.ST (STUArray, newListArray, getElems)
import Control.Monad.ST (ST, runST)

fauxsort [] = []
fauxsort (piv:xs) = fauxsort below ++ [piv] ++ fauxsort above
    where
    below = filter (< piv) xs
    above = filter (>= piv) xs

data QSortState = QSortState { current_piv_pos :: Int }

quicksort xs = runST $ do
    arr <- newListArray (0, right) xs
    _quicksort arr 0 right >> getElems arr
    where right = length xs - 1

-- in-place quicksort over [left, right]
_quicksort :: STUArray s Int Int -> Int -> Int -> ST s ()
_quicksort a left right
    | left < right = do
        piv <- unsafeRead a right
        pivpos <- partition piv left left right
        swap pivpos right
        _quicksort a left $ pivpos - 1
        _quicksort a (pivpos + 1) right
    | otherwise = return ()
    where
    partition !piv !pivpos !l !r
        | l < r = do
            v <- unsafeRead a l
            if v < piv
                then swap l pivpos >> partition piv (pivpos+1) (l+1) r
                else partition piv pivpos (l+1) r
        | otherwise = return pivpos
    swap i j = do
        temp <- unsafeRead a i
        unsafeRead a j >>= unsafeWrite a i
        unsafeWrite a j temp

{-
myqsort :: STUArray s Int Int -> Int -> Int -> ST s ()
myqsort a lo hi
    | lo < hi = do
        let lscan p h i
               | i < h = do
                   v <- unsafeRead a i
                   if p < v then return i else lscan p h (i+1)
               | otherwise = return i
            rscan p l i
               | l < i = do
                   v <- unsafeRead a i
                   if v < p then return i else rscan p l (i-1)
               | otherwise = return i
            swap i j = do
               v <- unsafeRead a i
               unsafeRead a j >>= unsafeWrite a i
               unsafeWrite a j v
            sloop p l h
               | l < h = do
                   l1 <- lscan p h l
                   h1 <- rscan p l1 h
                   if (l1 < h1) then (swap l1 h1 >> sloop p l1 h1) else return l1
               | otherwise = return l
        piv <- unsafeRead a hi
        i <- sloop piv lo hi
        swap i hi
        myqsort a lo (i-1)
        myqsort a (i+1) hi
    | otherwise = return ()

swapArray arr i j = do
    jval <- readArray arr j
    readArray arr i >>= writeArray arr j
    writeArray arr i jval
-}

main = print $ quicksort [256, 220..2]
