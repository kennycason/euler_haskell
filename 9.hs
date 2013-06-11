import Euler

triplets limit = [ a * b * c
                      | c <- [1..limit]
                      , b <- [1..(c-1)]
                      , a <- [1..(b-1)]
                      , a + b + c == limit
                      , a^2 + b^2 == c^2]

main = print (head (triplets 1000))
