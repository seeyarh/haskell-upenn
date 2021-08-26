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
