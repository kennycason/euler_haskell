import Euler

sumOfSquares :: Int -> Int
sumOfSquares n = sum (map (\i -> i^2) [1..n])

squareOfSums :: Int -> Int
squareOfSums n = (sum [1..n])^2

diff :: Int -> Int
diff n = (squareOfSums n) - (sumOfSquares n)

main = print (diff 100)
