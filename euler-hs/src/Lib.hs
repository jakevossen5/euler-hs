module Lib
    ( getNFibs
    , getUnlimitedFibs
    , getNextFib
    , isFactor
    , primeList
    , isPrime
    , lowerSq
    , isPalindrome
    , isIntPalindrome
    , getFactors
    , numFactors
    , sumOfDigits
    , factorial
    , lastN
    , simplifyFraction
    ) where
import Data.Char

simplifyFraction :: Integer -> Integer -> (Integer,Integer)
simplifyFraction x y
  | rem == 1 = (x,y)
  | otherwise = simplifyFraction (x `div` rem) (y `div` rem)
      where rem = gcd x y

-- https://stackoverflow.com/questions/17252851/how-do-i-take-the-last-n-elements-of-a-list
lastN n xs = drop (length xs - n) xs

factorial n = foldl (\acc x -> acc * x) 1 [1..n]

sumOfDigits n = sum $ map (digitToInt) (show $ n)

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

numFactors :: Integer -> Int
numFactors n = length $ getFactors n

-- http://rosettacode.org/wiki/Factors_of_an_integer
getFactors n =
  ds ++
  [ r
  | (d, 0) <- [divMod n r] 
  , r <-
     r :
     [ d
     | d > r ] ] ++
  reverse (map (n `div`) ds)
  where
    r = floor (sqrt (fromIntegral n))
    ds =
      [ i
      | i <- [1 .. r - 1] 
      , mod n i == 0 ]


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
