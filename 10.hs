import Euler

main = print (sum [p | p <- [2..1999999]
              , isPrime p])
