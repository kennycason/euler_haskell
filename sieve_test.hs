import Euler

-- works but needs to go from 1 to root(n), not root(n) to 2
primeSieve :: Int -> [Int]
primeSieve n = let upper = truncate (sqrt (fromIntegral n))
               in siever ([2] ++ [3,5..n]) upper
               where siever set 1 = []
                     siever set 2 = set
                     siever set n | isPrime (head set) = siever (filterMultiples set n) (n - 1)
                                  | otherwise = filterMultiples set n
                                  where filterMultiples set n = filter (\x -> x == n || (mod x n) /= 0) set
                             
                             
main = print (primeSieve 120)
