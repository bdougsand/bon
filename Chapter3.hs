module Main where

sieve [] = []
sieve (n : ns) =
    [n] ++ (sieve [m | m <- ns, m `mod` n /= 0])

primes :: [Int]
primes = sieve [2..]

godelize :: [Int] -> Int
godelize xs =
    product [p^x | (x, p) <- (zip xs primes)]

divs n x =
    let
        fs = takeWhile (\f -> n `mod` f == 0)
            (iterate (*x) x)
    in
        (length fs, x^(length fs))

dofactor fs 1 ps = fs
dofactor fs n [] = fs
dofactor fs n (p:ps) =
    let (count, pow) = divs n p in
        dofactor (fs ++ (take count (repeat p))) (n `div` pow) ps

factors n = dofactor [] n primes


main = putStrLn (show (factors 20))

-- main = putStrLn (show (take 2 (repeat 5)))λ>
