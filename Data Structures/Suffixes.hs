module suffixes
    where

suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes [x] = [[x]]
suffixes xxs@(x:xs) =  [xxs] ++ (suffixes xs) 