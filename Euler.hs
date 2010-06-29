{-# OPTIONS_GHC -Wall -Werror #-}
module Main where
import List

-- Current Project Euler max-project-number
max_problem_num :: Integer
max_problem_num = 4

-- Problem 1 - Add all the natural numbers below one thousand that are multiples of 3 or 5.
problem1 :: Integer
problem1 = sum (filter (\x -> (x `mod` 3 == 0 || x `mod` 5 == 0)) [1 .. 999])

-- Problem 2 -- Find the sum of all the even-valued terms in the sequence which do not exceed four million.
fibonacci_numbers :: [Integer]
fibonacci_numbers = 0 : 1 : zipWith (+) fibonacci_numbers (tail fibonacci_numbers)

problem2 :: Integer
problem2 = sum (fst (span (< 4000000) (filter even fibonacci_numbers)))

-- Problem 4 -- Find the largest palindrome made from the product of two 3-digit numbers.
digitArray :: Integer -> [Integer] -> [Integer]
digitArray n arr
	| n `div` 10 > 0 = digitArray (n `div` 10) ((n `rem` 10):arr)
	| otherwise = ((n `rem` 10):arr)

isMirror :: [Integer] -> Bool
isMirror arr
	| (length(arr) == 0) || (length(arr) == 1) = True
	| head(arr) == head(reverse(arr)) = isMirror (drop 1 (reverse (drop 1 arr)))
	| otherwise = False

isPalindrome :: Integer -> Bool
isPalindrome n = isMirror (digitArray n [])

largest_palindrome :: Integer
largest_palindrome = head( filter isPalindrome (reverse (sort productOfAllThreeDigitNumbers)))
	where
	productOfAllThreeDigitNumbers = [ x * y | x <- [999, 998 .. 100], y <- [999, 998 .. 100]]

-- Add new problems to this list as they are created
eulerEval :: Integer -> Integer
eulerEval 1 = problem1
eulerEval 2 = problem2
eulerEval 4 = largest_palindrome
eulerEval _ = 0

-- | main, do Nothing
main :: IO()
main = mapM_ print (map eulerEval [1 .. max_problem_num])
