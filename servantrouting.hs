{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}

import qualified Data.Map as M
import Data.Proxy
import GHC.TypeLits

data Trie a b = Trie (Maybe b) (M.Map a (Trie a b)) deriving Show

empty :: Trie a b
empty = Trie Nothing (M.empty)

tlookup :: Ord a => [a] -> Trie a b -> Maybe b
tlookup [] (Trie v _) = v
tlookup (x:xs) (Trie _ children) = case M.lookup x children of
    Nothing -> Nothing
    Just trie -> tlookup xs trie

insert :: Ord a => [a] -> b -> Trie a b -> Trie a b
insert [] v trie@(Trie (Just _) _)= trie
insert [] v (Trie Nothing children) = Trie (Just v) children
insert (x:xs) v (Trie val children) = case M.lookup x children of
    Nothing -> Trie val $ M.insert x (insert xs v $ Trie Nothing M.empty) children
    Just _ -> Trie val $ M.adjust (insert xs v) x children

infixr 5 :/
data (a :: Symbol) :/ (b :: k)  -- (k :: KnownSymbol) (j :: KnownSymbol)

class Makey t where
    listo :: Proxy t -> [String]

instance Makey () where
    listo _ = []

instance (KnownSymbol a, Makey b) => Makey (a :/ b) where
    listo (_ :: Proxy (a :/ b)) =
        symbolVal (Proxy :: Proxy a) : listo (Proxy :: Proxy b)

infixr 4 :+
data a :+ b

class TrieMakey t where
    treeo :: Proxy t -> Trie String ()

instance (Makey a, TrieMakey b) => TrieMakey (a :+ b) where
    treeo (_ :: Proxy (a :+ b)) =
        insert (listo (Proxy :: Proxy a)) () $ treeo (Proxy :: Proxy b)

instance Makey a => TrieMakey a where
    treeo prx = insert (listo prx) () empty

type API
    = ("subdir" :/ "subsubdir" :/ ())
    :+ ("subdir2" :/ "subsubdir2" :/ ())
    :+ ("subdir" :/ "subsubdir2" :/ ())
example = treeo (Proxy :: Proxy API)
