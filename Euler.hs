{-# OPTIONS_GHC -Wall -Werror #-}
module Main where
import List

-- Current Project Euler max-project-number
max_problem_num :: Integer
max_problem_num = 7

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
	| head(arr) == last(arr) = isMirror (drop 1 (reverse (drop 1 arr)))
	| otherwise = False

isPalindrome :: Integer -> Bool
isPalindrome n = isMirror (digitArray n [])

largest_palindrome :: Integer
largest_palindrome = head( filter isPalindrome (reverse (sort productOfAllThreeDigitNumbers)))
	where
	productOfAllThreeDigitNumbers = [ x * y | x <- [999, 998 .. 100], y <- [999, 998 .. 100]]

-- Problem 5 -- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20 ?
smallest_divisible_number :: Integer
smallest_divisible_number = foldl lcm 1 [1..20]

-- Problem 6 -- Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum
sq :: Integer -> Integer
sq x = x * x

sumsq_diff :: Integer
sumsq_diff = abs ((sq (sum [1..100])) - (sum (map sq [1..100])))

-- Problem 7 -- What is the 10001^(st) prime number ?
filter_primes :: Integer -> [Integer] -> [Integer] -> [Integer]
filter_primes _ _ [] = error "ran out of integers!"
filter_primes 0 currentlist _ = currentlist
filter_primes count currentlist (x:xs) = filter_primes (count-1) (x:currentlist) (filter (\n -> n `mod` x /= 0) xs)

prime_1001 :: Integer
prime_1001 = head(filter_primes 1001 [] [2..])

-- Add new problems to this list as they are created
eulerEval :: Integer -> Integer
eulerEval 1 = problem1
eulerEval 2 = problem2
eulerEval 4 = largest_palindrome
eulerEval 5 = smallest_divisible_number
eulerEval 6 = sumsq_diff 
eulerEval 7 = prime_1001
eulerEval _ = 0

-- | main, print out all known solutions
main :: IO()
main = mapM_ print (map eulerEval [1 .. max_problem_num])
