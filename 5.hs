import Euler

{--
full range: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
optimize array:
if n % 20 == 0, then n % 10 == 0 and n % 5 == 0 and n % 2 == 0,
therefore cut out 10, 5, and 2
[11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
--}
divides1to10 :: Int -> Bool
divides1to10 n = let list = [11..20]
                 in all (\i -> (mod n i) == 0) list
                 
divideLoop :: Int -> Int
divideLoop n | divides1to10 n = n
             | otherwise = divideLoop (n + 1)

main = print (divideLoop 2520)


