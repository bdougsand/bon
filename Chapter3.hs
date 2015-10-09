module Main where

sieve (n : ns) =
    [n] ++ (sieve [m | m <- ns, m `mod` n /= 0])

primes = sieve [2..]

godelize xs = 
    product [x^p | (x, p) <- (zip xs primes)]

main = putStrLn (show (godelize [1, 2, 3, 4]))