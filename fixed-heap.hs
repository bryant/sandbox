{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Control.Monad.ST (runST)
import Data.Vector.Generic (Vector, unsafeFreeze, thaw, freeze, (!?), length, (//))
import Data.Vector.Generic.Mutable (MVector, unsafeRead, unsafeWrite)
import qualified Data.Vector.Unboxed as UV
import qualified Control.Monad.State as S

import Prelude hiding (length, head)

type JID = Int
type Compat = Int
type Appointment = (JID, Compat)
type Heap = UV.Vector
type Roster = Heap Appointment

-- instance Eq Appointment where Appointment _ a == Appointment _ b = a == b

{-instance Ord Appointment where
    Appointment _ a < Appointment _ b = a < b
    Appointment _ a > Appointment _ b = a > b
-}

siftDown hp pos = case hp !? pos of
    Nothing -> hp
    Just val -> runST $ do
        ts <- thaw hp
        finalpos <- descend ts pos val
        unsafeWrite ts finalpos val
        unsafeFreeze ts
    where
    descend ts par val
        | par <= maxparent = do
            l <- unsafeRead ts $ leftchild par
            r <- unsafeRead ts $ rightchild par
            next' l r val
        | otherwise = return par
        where
        next' l r val
            -- match l first b/c r == l when rightchild > maxpos
            | min_ == l = unsafeWrite ts par l >> descend ts (leftchild par) val
            | min_ == r = unsafeWrite ts par r >> descend ts (rightchild par) val
            | otherwise = return par
            where min_ = minimum [l, r, val]

    maxpos = length hp - 1
    maxparent = (maxpos - 1) `div` 2
    leftchild = (1+) . (2*)
    rightchild p = min maxpos $ leftchild p + 1

data HireState = HireState { roster :: Roster }
type Hiring = S.State HireState

swapIn :: Appointment -> Hiring Appointment
swapIn newguy = do
    HireState r <- S.get
    S.put . HireState . flip siftDown 0 $ r // [(0, newguy)]
    return $ UV.head r
