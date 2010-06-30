{-# OPTIONS_GHC -Wall -Werror #-}
module Main where
import List

-- Current Project Euler max-project-number
max_problem_num :: Integer
max_problem_num = 8

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
filter_primes :: Integer -> Integer -> [Integer] -> Integer
filter_primes _ _ [] = error "Ran out of integers !!"
filter_primes count prod (number:numbers)
	| (gcd number prod) > 1 = filter_primes count prod numbers
	| (gcd number prod) == 1  = 
		if count > 0 then filter_primes (count -1) (prod * number) numbers
			      else number
	| otherwise = error "Impossible!"

prime_10001 :: Integer
prime_10001 = filter_primes 10000 2 [1..]

-- Problem 8 -- Find the greatest product of five consecutive digits in the 1000-digit number
maxArrayProduct :: [Integer] -> Integer
maxArrayProduct arr = maximum(map product (map (take 5) (tails arr)))

big1000num :: Integer
big1000num = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450

digitProduct :: Integer
digitProduct = maxArrayProduct (digitArray big1000num [])

-- Add new problems to this list as they are created
eulerEval :: Integer -> Integer
eulerEval 1 = problem1
eulerEval 2 = problem2
eulerEval 4 = largest_palindrome
eulerEval 5 = smallest_divisible_number
eulerEval 6 = sumsq_diff 
eulerEval 7 = prime_10001
eulerEval 8 = digitProduct
eulerEval _ = 0

-- | main, print out all known solutions
main :: IO()
main = mapM_ print (map eulerEval [1 .. max_problem_num])
