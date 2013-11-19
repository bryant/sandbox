data MyMaybe a = MyNothing | MyJust a deriving Show
-- MyJust :: a -> Maybe a

instance Monad (MyMaybe) where
    -- return :: a -> MyMaybe a
    return x = MyJust x

    -- (>>=) :: MyMaybe a -> (a -> MyMaybe b) -> MyMaybe b
    MyJust x >>= f = f x
    MyNothing >>= _ = MyNothing

-- data State s a = State (s -> (a, s))
-- instance Monad (State s) where
--     return :: a -> (State s) a
--     return x = State $ \r -> (x, r)
-- 
--     (>>=) :: (State s) a -> (a -> (State s) b) -> (State s) b
--     (State h) >>= f = State $ f h

type SimpleState s a = s -> (a, s)

returnSt :: a -> SimpleState s a
returnSt = (,)

bindSt :: SimpleState s a -> (a -> SimpleState s b) -> SimpleState s b
bindSt (SimpleState st a) f = f a)

------- more monad practice


father :: Person -> Maybe Person

(>>=) :: Maybe Person -> (Person -> Maybe Person) -> Maybe Person

bothGrandfathers :: Person -> Maybe (Person, Person)
bothGrandfathers p = do
    f <- father p
    ff <- father f
    m <- mother p
    mf <- father m   -- :: Person -> Maybe Person
    return (ff, mf)  -- :: Person -> Maybe (Person, Person)

p0 >>= pgen = state f
    where
    f st = runState (pgen (fst (runState p0 st))) (snd (runState p0 st))
