{-# LANGUAGE BangPatterns #-}

-- automatic differentiation test

module AutoDiff where

import qualified Data.List as L

{-
data BinPrim = Add | Sub | Prod | Div
    deriving Show

data UnPrim = Cos | Sin | Tan
  deriving Show

-- ssa

data Expr a
    = Call Fn (Expr a) (Expr a)
    | BinOp BinPrim (Expr a) (Expr a)
    | UnOp UnPrim (Expr a)
    | Var a
    | Const Double
    deriving Show

-- | automatic differentiation
ad :: a -> Expr a -> Expr a
ad diff (Var vname)
  | vname == diff = Const 1.0
  | otherwise = 
ad diff (Const _) = Const 0
-}

data Dual a = Dual { real :: a, diff :: a }
    deriving (Show, Eq)

instance Num a => Num (Dual a) where
    (Dual a a_) + (Dual b b_) = Dual (a + b) (a_ + b_)

    (Dual a a_) - (Dual b b_) = Dual (a - b) (a_ - b_)

    (Dual a a_) * (Dual b b_) = Dual (a * b) (a*b_ + a_*b)

    negate (Dual a a_) = Dual (negate a) (negate a_)

    abs _ = undefined

    signum = undefined

    fromInteger n = Dual (fromInteger n) 0

instance Floating a => Floating (Dual a) where
    pi = Dual pi 0
    exp (Dual a a_) = Dual (exp a) (a_ * exp a)
    log (Dual a a_) = Dual (log a) (a_ / a)
    sin (Dual a a_) = Dual (sin a) (a_ * cos a)
    --cos
    --sinh
    --cosh
    --asinh
    --acosh
    --atanh

instance Fractional a => Fractional (Dual a) where
    (Dual a a_) / (Dual b b_) = Dual (a / b) (num / b ^ 2)
        where num = a_*b - a*b_

    fromRational r = Dual (fromRational r) 0

calc :: Num a => (Dual a -> Dual a) -> a -> Dual a
calc f x = f $ Dual x 1

iterapp :: Int -> (a -> a) -> a -> a
iterapp 0 _ !x = x
iterapp !n f !x = f $ iterapp (n - 1) f x

-- | symbolically differentiating this would explode
example :: Floating a => a -> a
example = iterapp 256 (\x -> log (x + exp x))
