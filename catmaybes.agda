const : {A B : Set} -> A -> B -> A
const x y = x

data List (A : Set) : Set where
    Nil : List A
    Cons : A -> List A -> List A

map : {A B : Set} -> (A -> B) -> List A -> List B
map f Nil = Nil
map f (Cons x xs) = Cons (f x) (map f xs)

data Maybe (A : Set) : Set where
    Nothing : Maybe A
    Just : A -> Maybe A

catMaybes : {A : Set} -> List (Maybe A) -> List A
catMaybes Nil = Nil
catMaybes (Cons Nothing xs) = catMaybes xs
catMaybes (Cons (Just v) xs) = Cons v (catMaybes xs)

data Eq {A : Set} (x : A) : A -> Set where
    refl : Eq x x

zq : {A : Set} -> (xs : List A) ->
     Eq (Nil {A}) (catMaybes (map (const Nothing) xs))
zq Nil = refl
zq (Cons x xs) = zq xs

-- extraneous

cong : {A B : Set} -> {x y : A} -> (f : A -> B) -> Eq x y -> Eq (f x) (f y)
cong f refl = refl

list_of_nothings : {A : Set} -> List A -> List (Maybe A)
list_of_nothings Nil = Nil
list_of_nothings (Cons x xs) = Cons Nothing (list_of_nothings xs)

nothings_reduce_to_nil : {A : Set} -> (xs : List A) ->
                         Eq Nil (catMaybes (list_of_nothings xs))
nothings_reduce_to_nil Nil = refl
nothings_reduce_to_nil (Cons x xs) = nothings_reduce_to_nil xs

map_const_makes_nothings : {A : Set} -> (xs : List A) ->
                           Eq (map (const Nothing) xs) (list_of_nothings xs)
map_const_makes_nothings Nil = refl
map_const_makes_nothings (Cons x xs) =
    cong (Cons Nothing) (map_const_makes_nothings xs)
