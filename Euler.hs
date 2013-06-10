module Euler
    (divisible
    ,fib
    ,fibs
    ,fibm
)
where

-- divisible()
divisible :: [Int] -> Int -> Bool
divisible divisors n = any (\divisor -> (mod n divisor) == 0) divisors


-- fibonacci sequence, generate n-th term
fib :: Int -> Integer
fib = (map fib [0 ..] !!)
   where fib 0 = 1
         fib 1 = 1
         fib n = fib (n-2) + fib (n-1)
   
-- fibonacci sequence, generate n terms      
fibs :: Int -> [Integer]
fibs terms = [a | (a,b) <- take 
                               terms 
                               (iterate 
                                     (\(a,b) -> (b, a+b)) 
                                     (0,1))]
                               
-- fibonacci sequence max terms, generate terms up to max term
fibm :: Int -> [Integer]
fibm max = [a | (a,b) <- takeWhile 
                                (\(a,b) -> a <= fromIntegral(max)) 
                                (iterate 
                                      (\(a,b) -> (b, a+b)) 
                                      (0,1))]                   
 
