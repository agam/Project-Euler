{-# OPTIONS_GHC -Wall -Werror #-}
module Main where
import List

-- Current Project Euler max-project-number
max_problem_num :: Integer
max_problem_num = 11

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

-- Problem 9 -- There exists exactly one Pythagorean triplet for which a + b + c = 1000. Find the product abc.
allPythTriplets :: [(Integer, Integer, Integer)]
allPythTriplets = [(x*x - y*y,2*x*y,x*x + y*y) | x <- [1 ..], y<-[1..(x-1)]]

findPythTriplet :: Integer
findPythTriplet = let
	triplet_product::(Integer, Integer, Integer) -> Integer
	triplet_product (a,b,c) = a*b*c
	triplet_sum :: (Integer, Integer, Integer) -> Integer
	triplet_sum (a,b,c) = a + b + c
	in
	triplet_product (head( filter (\x -> ((triplet_sum x) == 1000)) allPythTriplets))

-- Problem 10 -- Find the sum of all the primes below two million
-- TODO(agam): Scope for refactoring with problem 7 here. Also, this is extremely brute force and takes around 15 mins (!)
sum_primes :: Integer -> Integer-> [Integer] -> Integer
sum_primes runningsum _ [] = runningsum
sum_primes runningsum prod (number:numbers)
	| (gcd number prod) > 1 = sum_primes runningsum prod numbers
	| (gcd number prod) == 1 = sum_primes (runningsum + number) (prod * number) numbers
	| otherwise = error "Impossible !"

prime_sum :: Integer
prime_sum = sum_primes 0 1 [2..2000000]

-- Problem 11 -- Find the 4 digits with the largest product in any direction
four_num_mat :: [[Integer]]
four_num_mat = [[08, 02, 22, 97, 38, 15, 00, 40, 00, 75, 04, 05, 07, 78, 52, 12, 50, 77, 91, 08],
		[49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48, 04, 56, 62, 00],
		[81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30, 03, 49, 13, 36, 65],
		[52, 70, 95, 23, 04, 60, 11, 42, 69, 24, 68, 56, 01, 32, 56, 71, 37, 02, 36, 91],
		[22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80],
		[24, 47, 32, 60, 99, 03, 45, 02, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50],
		[32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70],
		[67, 26, 20, 68, 02, 62, 12, 20, 95, 63, 94, 39, 63, 08, 40, 91, 66, 49, 94, 21],
		[24, 55, 58, 05, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72],
		[21, 36, 23, 09, 75, 00, 76, 44, 20, 45, 35, 14, 00, 61, 33, 97, 34, 31, 33, 95],
		[78, 17, 53, 28, 22, 75, 31, 67, 15, 94, 03, 80, 04, 62, 16, 14, 09, 53, 56, 92],
		[16, 39, 05, 42, 96, 35, 31, 47, 55, 58, 88, 24, 00, 17, 54, 24, 36, 29, 85, 57],
		[86, 56, 00, 48, 35, 71, 89, 07, 05, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58],
		[19, 80, 81, 68, 05, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77, 04, 89, 55, 40],
		[04, 52, 08, 83, 97, 35, 99, 16, 07, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66],
		[88, 36, 68, 87, 57, 62, 20, 72, 03, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69],
		[04, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18, 08, 46, 29, 32, 40, 62, 76, 36],
		[20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74, 04, 36, 16],
		[20, 73, 35, 29, 78, 31, 90, 01, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57, 05, 54],
		[01, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52, 01, 89, 19, 67, 48]]

mat_element :: [[Integer]] -> Int -> Int -> Integer
mat_element [] _ _ = error "Empty matrix passed in"
mat_element mat i j = (mat !! i) !! j

compute_dir_prod :: [[Integer]] -> Int -> Int -> Integer
compute_dir_prod [] _ _ = error "Empty matrix !"
compute_dir_prod mat i j = maximum [
		product [mat_element mat i k | k <- [j-3 .. j]],
		product [mat_element mat i k | k <- [j .. j+3]],
		product [mat_element mat k j | k <- [i-3 .. i]],
		product [mat_element mat k j | k <- [i .. i+3]],
		product [mat_element mat (i-k) (j-k) | k <- [0 .. 3]],
		product [mat_element mat (i+k) (j+k) | k <- [0 .. 3]],
		product [mat_element mat (i-k) (j+k) | k <- [0 .. 3]],
		product [mat_element mat (i+k) (j-k) | k <- [0 .. 3]]
		]

get_all_prod :: [[Integer]] -> [[Integer]]
get_all_prod mat = [[ compute_dir_prod mat i j | j <- [3 .. ((length (mat !! i)) - 4)]] | i <- [3.. ((length mat) - 4)]]

largest_mat_element :: [[Integer]] -> Integer
largest_mat_element mat = maximum (map maximum mat)

four_num_prod :: Integer
four_num_prod = largest_mat_element (get_all_prod four_num_mat)

-- Add new problems to this list as they are created
eulerEval :: Integer -> Integer
eulerEval 1 = problem1
eulerEval 2 = problem2
eulerEval 4 = largest_palindrome
eulerEval 5 = smallest_divisible_number
eulerEval 6 = sumsq_diff 
eulerEval 7 = prime_10001
eulerEval 8 = digitProduct
eulerEval 9 = findPythTriplet
eulerEval 10 = prime_sum
eulerEval 11 = four_num_prod
eulerEval _ = 0

-- | main, print out all known solutions
main :: IO()
main = mapM_ print (map eulerEval [1 .. max_problem_num])
