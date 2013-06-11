module Euler
    (divisible
    ,fib
    ,fibs
    ,fibm
    ,isPrime
    ,factors
)
where


-- divisible()
divisible :: [Int] -> Int -> Bool
divisible divisors n = any (\divisor -> (mod n divisor) == 0) divisors


-- fib() - fibonacci sequence, generate n-th term
fib :: Int -> Integer
fib = (map fib [0 ..] !!)
   where fib 0 = 1
         fib 1 = 1
         fib n = fib (n-2) + fib (n-1)
   
-- fibs() - fibonacci sequence, generate n terms      
fibs :: Int -> [Integer]
fibs terms = [a | (a,b) <- take 
                               terms 
                               (iterate 
                                     (\(a,b) -> (b, a+b)) 
                                     (0,1))]
                               
-- fibm() - fibonacci sequence max terms, generate terms up to max term
fibm :: Int -> [Integer]
fibm max = [a | (a,b) <- takeWhile 
                                (\(a,b) -> a <= fromIntegral(max)) 
                                (iterate 
                                      (\(a,b) -> (b, a+b)) 
                                      (0,1))]                   
 
 
-- isPrime()
isPrime :: Int -> Bool
isPrime n | n <= 1 = False
          | otherwise = let root = sqrt (fromIntegral n)
                        in not (any 
                                   (\i -> (mod n i) == 0) 
                                   [2..truncate(root)])
  
                                                    
-- factors()
factors :: Int -> [Int]
factors n = [x | x <- [2..s], (mod n x) == 0]
    where s = floor (sqrt (fromIntegral n))
                        
