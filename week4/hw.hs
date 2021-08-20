{-# OPTIONS -Wall #-}

import Data.List

-- Ex 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

newFun1 :: [Integer] -> Integer
newFun1 = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

collatz :: Integral a => a -> a
collatz n
  | even n = n `div` 2
  | otherwise = 3 * n + 1

newFun2 :: Integer -> Integer
newFun2 = sum . takeWhile (/= 1) . iterate collatz

-- Ex 2
data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show)

foldTree :: [a] -> Tree a
foldTree = foldr insertBalanced Leaf

insertBalanced :: a -> Tree a -> Tree a
insertBalanced x Leaf = Node 0 Leaf x Leaf
insertBalanced x (Node height left@Leaf v right) = do
  let newLeft = insertBalanced x left
  let newHeight =
        case right of
          Leaf -> height + 1
          Node {} -> height
  Node newHeight newLeft v right
insertBalanced x (Node height left v right@Leaf) = do
  let newRight = insertBalanced x right
  let newHeight =
        case left of
          Leaf -> height + 1
          Node {} -> height
  Node newHeight left v newRight
insertBalanced x (Node _ left@(Node heightLeft _ _ _) v right@(Node heightRight _ _ _)) =
  if heightLeft > heightRight
    then do
      let newRight@(Node newHeightRight _ _ _) = insertBalanced x right
      let newHeight = 1 + max heightLeft newHeightRight
      Node newHeight left v newRight
    else do
      let newLeft@(Node newHeightLeft _ _ _) = insertBalanced x left
      let newHeight = 1 + max newHeightLeft heightRight
      Node newHeight newLeft v right

-- Ex 3
xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []

-- Ex 4
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

nonPrimes :: Integer -> [Integer]
nonPrimes n =
  filter
    (<= n)
    (map
       (\(x, y) -> x + y + (2 * x * y))
       (filter (uncurry (<=)) (cartProd [1 .. n + 1] [1 .. n + 1])))

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2 * x + 1) ([1 .. n] \\ nonPrimes n)
