module BackTrackSolver where

import Debug.Trace (trace)

type Literal = Int
type Clause = [Literal]
type SolutionSet = [Literal]

solns `disproves` l = -l `elem` solns

solve' :: SolutionSet -> [Clause] -> Maybe SolutionSet
solve' solns [] = Just solns
solve' solns (c:cs) = trace msg $ try_branches c
    where
    msg = show solns ++ " -?-> " ++ show (c:cs)
    try_branches [] = Nothing
    try_branches (l:ls) = case solns `disproves` l of
        False -> case solve' solns' cs of
            Nothing -> try_branches ls
            Just yay -> Just yay
            where solns' = if l `elem` solns then solns else l : solns
        True -> try_branches ls

solve = solve' []
