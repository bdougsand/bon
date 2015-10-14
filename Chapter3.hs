module Main where

sieve :: Integral a => [a] -> [a]
sieve [] = []
sieve (n : ns) =
    [n] ++ (sieve [m | m <- ns, m `mod` n /= 0])

primes :: [Integer]
primes = sieve [2..]

godelize :: Integral b => [b] -> Integer
godelize xs =
    product [p^x | (x, p) <- (zip xs primes)]

divs :: Integral a => a -> a -> (Int, a)
divs n x =
    let
        fs = takeWhile (\f -> n `mod` f == 0)
            (iterate (*x) x)
    in
        (length fs, x^(length fs))

dofactor :: Integral a => [a] -> a -> [a] -> [a]
dofactor fs 1 _ = fs
dofactor fs _ [] = fs
dofactor fs n (p:ps) =
    let (count, pow) = divs n p in
        dofactor (fs ++ (take count (repeat p))) (n `div` pow) ps

factors :: Integer -> [Integer]
factors n = dofactor [] n primes


countHead :: Eq a => [a] -> [a] -> [Int]
countHead [] _ = []
countHead _ [] = []
countHead (v : vs) ns =
          [length (takeWhile (== v) ns)] ++ (countHead vs (dropWhile (== v) ns))


ungodelize :: Integer -> [Int]
ungodelize n = countHead primes (factors n)


main :: IO ()
main = putStrLn (show (factors 20))
