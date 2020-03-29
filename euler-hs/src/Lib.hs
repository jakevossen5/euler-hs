module Lib
    ( getNFibs, getUnlimitedFibs, isFactor, primeList, isPrime, lowerSq, isPalindrome, isIntPalindrome
    ) where


isPalindrome :: [Char] -> Bool
isPalindrome x = x == (reverse x)

isIntPalindrome :: Integer -> Bool
isIntPalindrome x = isPalindrome (show x)

lowerSq :: Integer -> Integer
lowerSq x = floor (sqrt (fromIntegral x))

-- prime code from here: https://wiki.haskell.org/Prime_numbers#Turner.27s_sieve_-_Trial_division
noDivs n ds = foldr (\d r -> d*d > n || (rem n d > 0 && r)) True ds

primeList  = 2 : 3 : filter (`noDivs` tail primeList) [5,7..]

isPrime n = n > 1 && noDivs n primeList

isFactor :: Integer -> Integer -> Bool
isFactor x y = x `mod` y == 0
-- getUnlimitedFibs :: [Int]
-- getUnlimitedFibs xs = [1, 1] ++ getUnlimitedFibs(xs ++ [getNextFib xs])
getUnlimitedFibs :: Integer -> Integer -> [Integer]
getUnlimitedFibs a b = a:getUnlimitedFibs b (a+b)

getNFibs :: Integer -> [Integer] -> [Integer]
getNFibs 0 xs = xs
getNFibs n xs = (getNFibs (n - 1)) (xs ++ [(getNextFib xs)])

getNextFib :: [Integer] -> Integer
getNextFib xs = (last xs) + (last (init xs))

someFunc :: IO ()
someFunc = putStrLn "someFunc"
