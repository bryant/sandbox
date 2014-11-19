-- generate permutations with steinhaus algo


perms_of [] = [[]]
perms_of [x] = [[x]]
perms_of (x:xs) = concatMap (iter x) $ perms_of xs

iter :: a -> [a] -> [[a]]
iter x xs = [insert_at pos x xs | pos <- [0..length xs]]

insert_at :: Int -> a -> [a] -> [a]
insert_at pos e es = take pos es ++ e : drop pos es
