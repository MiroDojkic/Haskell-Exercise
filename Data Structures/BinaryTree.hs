module BinaryTree
(Tree(Branch),
	sumNodes,
	multiplyNodes) 
where

data Tree a = Empty
		| Branch a (Tree a) (Tree a)
		deriving (Show, Eq)


sumNodes :: Num a => Tree a -> a
sumNodes Empty = 0
sumNodes (Branch x t1 t2) = x + sumNodes(t1) + sumNodes(t2)

multiplyNodes :: Num a => Tree a -> a
multiplyNodes Empty = 1
multiplyNodes (Branch x t1 t2) = x * multiplyNodes(t1) * multiplyNodes(t2)