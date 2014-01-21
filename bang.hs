{-# LANGUAGE BangPatterns #-}
import Data.List (foldl')
data Pair a = Pair !a !a deriving Show
ave = foldl' f $ Pair 0 1
    where f (Pair acc len) x = Pair (acc+(x-acc) / len) (len+1)
