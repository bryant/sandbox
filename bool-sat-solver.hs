--module BackTrackSolver where

import Debug.Trace (trace)
import CNFParse (parse_dimacs, Clause, SolutionSet)

solns `disproves` l = -l `elem` solns

solve' :: SolutionSet -> [Clause] -> Maybe SolutionSet
solve' solns [] = Just solns
#ifdef DEBUG
solve' solns (c:cs) = trace msg $ try_branches c
#else
solve' solns (c:cs) = try_branches c
#endif
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

main = do
    rv <- parse_dimacs `fmap` getContents
    case rv of
        Left e -> print e
        Right (_, _, f) -> print $ solve f
