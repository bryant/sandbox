{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators #-}

module MultiEither where

type family MultiEither (p :: [*]) where
    MultiEither [a, b] = Either a b
    MultiEither ('(:) t ts) = Either t (MultiEither ts)

type Example a = MultiEither [Int, Bool, Char, [a]]

w, x, y, z :: Example a
w = Left 3
x = Right $ Left False
y = Right . Right $ Left 'y'
z = Right . Right $ Right []
