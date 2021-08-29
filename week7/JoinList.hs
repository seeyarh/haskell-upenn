{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module JoinList where

import Buffer
import Scrabble
import Sized

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- Ex 1
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
j1 +++ j2 = Append (tag j1 <> tag j2) j1 j2

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- Ex 2
(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i
  | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i - 1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ n (Single m a)
  | Size (n + 1) == size m = Just a
  | otherwise = Nothing
indexJ n (Append m leftJ rightJ)
  | Size (n + 1) > sizeL = indexJ (n - getSize sizeL) rightJ
  | otherwise = indexJ n leftJ
  where
    sizeL = size $ tag leftJ
    sizeR = size $ tag rightJ

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ 0 jl = jl
dropJ _ (Single _ _) = Empty
dropJ n (Append m leftJ rightJ)
  | Size n >= sizeL = dropJ (n - getSize sizeL) rightJ
  | otherwise = dropJ n leftJ
  where
    sizeL = size $ tag leftJ
    sizeR = size $ tag rightJ

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ 0 jl = Empty
takeJ _ jl@(Single _ _) = jl
takeJ n (Append m leftJ rightJ)
  | Size n > sizeL = leftJ +++ takeJ (n - getSize sizeL) rightJ
  | otherwise = takeJ n leftJ
  where
    sizeL = size $ tag leftJ
    sizeR = size $ tag rightJ

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

halve :: [a] -> ([a], [a])
halve xs = splitAt s xs
  where
    s = length xs `div` 2

fromLines :: [String] -> JoinList (Score, Size) String
fromLines [] = Empty
fromLines [s] = Single (scoreString s, 1) s
fromLines xs = Append (sc, si) left right
  where
    halves = halve xs
    left = fromLines (fst halves)
    right = fromLines (snd halves)
    m = tag left <> tag right
    sc = fst m
    si = snd m

instance (Buffer (JoinList (Score, Size) String)) where
  toString Empty = ""
  toString (Single _ s) = s
  toString (Append _ left right) = toString left ++ toString right
  fromString s = fromLines (lines s)
  line = indexJ
  replaceLine _ l Empty = Single (scoreString l, 1) l
  replaceLine n l (Single _ _) = Single (scoreString l, 1) l
  replaceLine n l (Append m leftJ rightJ)
    | Size (n + 1) > sizeL = leftJ +++ replaceLine (n - getSize sizeL) l rightJ
    | otherwise = replaceLine n l leftJ +++ rightJ
    where
      sizeL = size $ snd $ tag leftJ
      sizeR = size $ snd $ tag rightJ
  numLines Empty = 0
  numLines (Single (_, n) _) = getSize n
  numLines (Append (_, n) _ _) = getSize n
  value Empty = 0
  value (Single (s, _) _) = getScore s
  value (Append (s, _) _ _) = getScore s
