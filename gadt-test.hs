{-# LANGUAGE GADTs, KindSignatures #-}

data Empty
data NonEmpty

data SafeList :: * -> * -> * where
    Nil :: SafeList a Empty
    Cons :: a -> SafeList a b -> SafeList a c

instance Show a => Show (SafeList a b) where
    show Nil = "[]"
    show (Cons a rest) = show a ++ " : " ++ show rest

safeHead :: SafeList a NonEmpty -> a
safeHead (Cons a _) = a
