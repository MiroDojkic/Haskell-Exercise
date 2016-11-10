module Suffixes
    where

suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes [a] = [[a]]
suffixes xxs@(x:xs) =  [xxs] ++ (suffixes xs) 