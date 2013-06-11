import Euler

main = print (maximum 
                [z | y<-[1..999], x<-[y..999], 
                     let z = x * y, 
                     let r = reversei z, r == z])
