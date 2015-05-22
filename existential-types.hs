{-# LANGUAGE ExistentialQuantification #-}

data D = forall a. D a
-- D :: forall a. a -> D

v :: D
v = D True
-- Translating to Pierce's notation:
-- or, u = {*Bool, True} as {exists X, forall X. X}
-- D :: exists x . x -> D aka {exists X, forall X. X} -> D
-- v = D $ {*Bool, True} as {exists X, forall X. X}


{-
or:
a :: exists x . x -> x
a = \x -> x + 3
or:
a = {*Int, \x -> x + 3} as {exist X, forall X. X -> X}
-}
