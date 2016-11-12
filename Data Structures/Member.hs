module BinaryTree
    where

data Tree a = Empty | Node (Tree a) a (Tree a)

member :: Ord a => a -> Tree a -> Bool
member x Empty = False
member x (Node t1 y t2)
    | y <= x = go x y t2
    | otherwise = go x y t1
    where
        go :: Ord a => a -> a -> Tree a -> Bool
        go x y Empty = x == y
        go x y (Node t1 z t2)
            | z <= x = go x z t2
            | otherwise = go x y t1
