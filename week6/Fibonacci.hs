{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

import Data.Bits

-- Ex 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

-- Ex 2
fibs2 :: [Integer]
fibs2 = map fst (scanl (\(f1, f2) _ -> (f2, f1 + f2)) (0, 1) [1 ..])

-- Ex 3
data Stream a =
  Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a s) = a : streamToList s

instance Show a => Show (Stream a) where
  show s = show (take 20 (streamToList s))

-- Ex 4
streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a s) = Cons (f a) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons fSeed (streamFromSeed f fSeed)
  where
    fSeed = f seed

-- Ex 5
nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x s1) (Cons y s2) =
  Cons x (Cons y (interleaveStreams s1 s2))

ruler :: Stream Integer
ruler =
  streamMap
    (fromIntegral . floor . logBase 2 . fromIntegral)
    (streamMap (\n -> n .&. complement (n - 1)) nats)

-- Ex 6
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

streamMult :: Integer -> Stream Integer -> Stream Integer
y `streamMult` (Cons x sx) = Cons (x * y) (y `streamMult` sx)

streamDiv :: Integer -> Stream Integer -> Stream Integer
-- equiv to (1/y) * sx
y `streamDiv` (Cons x sx) = Cons (x `div` y) (y `streamDiv` sx)

instance Num (Stream Integer) where
  fromInteger x = Cons x (streamRepeat 0)
  negate (Cons x sx) = Cons (negate x) (negate sx)
  (Cons x sx) + (Cons y sy) = Cons (x + y) (sx + sy)
  sx@(Cons x sx') * sy@(Cons y sy') =
    Cons (x * y) ((x `streamMult` sy') + (sx' * sy))

instance Fractional (Stream Integer) where
  sx@(Cons x sx') / sy@(Cons y sy') =
    Cons (x `div` y) (y `streamDiv` (sx' - (sx / sy) * sy'))

fibs3 :: Stream Integer
fibs3 = x / (fromInteger 1 - x - x ^ 2)

-- Ex 7
data Matrix =
  Matrix
    { x11 :: Integer
    , x12 :: Integer
    , x21 :: Integer
    , x22 :: Integer
    }
  deriving (Show)

instance Num Matrix where
  (Matrix m11 m12 m21 m22) * (Matrix n11 n12 n21 n22) =
    Matrix
      { x11 = m11 * n11 + m12 * n21
      , x12 = m11 * n12 + m12 * n22
      , x21 = m21 * n11 + m22 * n21
      , x22 = m21 * n12 + m22 * n22
      }

f :: Matrix
f = Matrix 1 1 1 0

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 1 = 1
fib4 n = x12 (f ^ n)
