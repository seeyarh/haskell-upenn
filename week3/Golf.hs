module Golf where

import Data.List

-- Ex 1
skips :: [a] -> [[a]]
skips [] = []
skips [x] = [[x]]
skips xs = map (skipN xs) [1 .. length xs]

shouldInclude :: Int -> (Int, a) -> Bool
shouldInclude n (i, _) = i `mod` n == 0

skipN :: [a] -> Int -> [a]
skipN xs 0 = xs
skipN xs n = map snd (filter (shouldInclude n) (zip [1 ..] xs))

-- Ex 2
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs)
  | y > x && y > z = y : localMaxima (y : z : xs)
  | otherwise = localMaxima (y : z : xs)
localMaxima xs = []

-- Ex 3
summary = "==========\n0123456789\n"

histogram :: [Integer] -> String
histogram xs = histogramCounts (countOccurrences xs) ++ "\n" ++ summary

countOccurrences :: [Integer] -> [Integer]
countOccurrences xs = [count n xs | n <- [0 .. 10]]

count :: Integer -> [Integer] -> Integer
count n = genericLength . filter (== n)

decrementIfNotZero xs = [max (x - 1) 0 | x <- xs]

getLineCount [] = ""
getLineCount (x:xs)
  | x > 0 = "*" ++ getLineCount xs
  | otherwise = " " ++ getLineCount xs

histogramCounts :: [Integer] -> String
histogramCounts xs
  | all (== 0) xs = ""
  | otherwise =
    histogramCounts (decrementIfNotZero xs) ++ "\n" ++ getLineCount xs
