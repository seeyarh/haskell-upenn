toDigitsHelper :: Integer -> Integer -> [Integer]
toDigitsHelper x n
    | x `div` n == 0 = []
    | otherwise =  toDigitsHelper x (n * 10) ++ [(x `div` n) `mod` 10]

toDigits :: Integer -> [Integer]
toDigits x = toDigitsHelper x 1

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse [if even (i + 1) then x*2 else x | (i, x) <- zip [0..] (reverse xs)]

sumDigits :: [Integer] -> Integer
sumDigits xs = sum (concat [toDigits x | x <- xs])

validate :: Integer -> Bool
validate x = sumDigits(doubleEveryOther (toDigits x)) `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi nDiscs source destination temp
    | nDiscs == 1 = [(source, destination)]
    | otherwise = hanoi (nDiscs - 1) source temp destination ++ [(source, destination)] ++ hanoi (nDiscs - 1) temp destination source
