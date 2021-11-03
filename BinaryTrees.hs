module Tut5 where

data BinTree a = Empty | Node (BinTree a) a (BinTree a) deriving (Show)

treeDbl :: Num n => BinTree n -> BinTree n
treeDbl Empty               = Empty
treeDbl (Node left x right) = Node (treeDbl left) (2*x) (treeDbl right)
--applied left to right

zeroSnd :: Num n => BinTree (k,n) -> BinTree (k,n)
zeroSnd Empty = Empty
zeroSnd (Node left (s,x) right) = Node (zeroSnd left) (s,0) (zeroSnd right)
--sets 2nd component in tree to 0

incrFst :: Num k => BinTree (k,n) -> BinTree (k,n)
incrFst Empty = Empty
incrFst (Node left (s,x) right) = Node (incrFst left) (s+1,x) (incrFst right)
--increments the first component


treeSum :: Num n => BinTree n -> n
treeSum Empty = 0
treeSum (Node left x right) = treeSum left + x + treeSum right

treeProd :: Num n => BinTree n -> n
treeProd Empty = 1
treeProd (Node left x right) = treeSum left * x * treeSum right

treeListSnd :: BinTree (k,d) -> [d]
treeListSnd Empty = []
treeListSnd (Node left (s,x) right) = treeListSnd left ++ x : treeListSnd right
