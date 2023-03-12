-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module PracticeTest (checksum , golfScorer, highlyDivisible, largestOddFactor, equals, babylonianPalindromes) where

import Types

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

{- Question 1 -}
checksum :: Integral a => [a] -> Bool
checksum l = (length l == 8) && ((sum l) `mod` 11 == 0)  

{- Question 2 -}
golfScorer :: Integer -> Integer -> Integer
golfScorer p s  | s == 1 = 5 
                | p - s >= 2 = 4 
                | p - s == 1 = 3
                | p - s == 0 = 2
                | p - s == -1 = 1
                | otherwise = 0

{- Question 3 -}

factorsPT :: Int -> [Int]
factorsPT n = [x | x <- [1..n], n `mod` x == 0]

factorsUntil12 :: Int -> [Int]
factorsUntil12 n = [a | a <- [1..12], a `elem` (factorsPT n)]


highlyDivisible :: Int -> [Int]
highlyDivisible n = [[a | a <- [2..], factorsUntil12 a == [1..12]] !! (n-1)]

largestOddFactor :: Int -> [Int]
largestOddFactor n = [last(filter odd (factorsPT n))]

  
{- Question 4 -}
equals :: (Enum a, Bounded a, Eq b) => (a -> b) -> (a -> b) -> Bool
equals = undefined
--equals f1 f2 = (f1 (maxBound(a)) == f2 (maxBound(a))) && (f1 (minBound(a)) == f2 (minBound(a))) && (f1 (succ(a)) == f2 (succ(a))) && (f1 (pred(a)) == f2 (pred(a))) where a :: Enum

--equals f1 f2 = (f1(a1) == f2(a2)) where ((maxBound(a1) == maxBound(a2)) && (minBound(a1) == minBound(a2)) && (pred(a1) == pred(a2)) && (succ(a1) == succ(a2)))

{- Question 5 -}

--babylonianPalindromes :: [Integer]
--babylonianPalindromes = [a :: Integer | a <- [1..], isPalindrome(numToBase60 a)]

--isPalindrome :: [Integer] -> Bool
--isPalindrome a = a == reverse(a)

--numToBase60 :: Integer -> [Integer]
--numToBase60 0 = []
--numToBase60 n = removeFirstZero( n `mod` 60 : numToBase60 (n `div` 60))

--removeFirstZero :: [Integer] -> [Integer]
--removeFirstZero [] = []
--removeFirstZero (0:xs) = xs
--removeFirstZero (x:xs) = (x:xs)

babylonianPalindromes :: [Integer]
babylonianPalindromes = [a | a <- [1..], isPalindrome(numToBase60 a), length (numToBase60 a) > 1]

isPalindrome :: [Integer] -> Bool
isPalindrome a = a == reverse(a)

numToBase60 :: Integer -> [Integer]
numToBase60 0 = []
numToBase60 n = reverse( n `mod` 60 : numToBase60 (n `div` 60))

removeFirstZero :: [Integer] -> [Integer]
removeFirstZero [] = []
removeFirstZero (0:xs) = xs
removeFirstZero (x:xs) = (x:xs)