import Euler

nthPrime :: Int -> Int -> Int
nthPrime n k | k == 10001 = n - 1
             | isPrime n = nthPrime (n + 1) (k + 1)
             | otherwise = nthPrime (n + 1) k

main = print (nthPrime 1 0)

