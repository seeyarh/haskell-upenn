toDigitsHelper :: Integer -> Integer -> [Integer]
toDigitsHelper x n
    | x `div` n == 0 = []
    | otherwise =  (toDigitsHelper (x) (n * 10)) ++ [(x `div` n) `mod` 10]

toDigits :: Integer -> [Integer]
toDigits x = toDigitsHelper x 1

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)
