{-# OPTIONS_GHC -Wall -Werror #-}
module Euler where
import System.IO

-- Current Project Euler max-project-number
--MAX_PROBLEM_NUM = 1 :: Integer

-- Problem 1 - Add all the natural numbers below one thousand that are multiples of 3 or 5.
problem1 :: (Integer a)
problem1 = sum [x | x <- [1..1000], x `mod` 3 == 0, x `mod` 5 == 0]

-- Add new problems to this list as they are created
eulerEval :: (Integer a) => a -> a
eulerEval 1 = problem1
eulerEval _ = 0

-- | main, do Nothing
main :: IO()
main = map print $ eulerEval [x | x <- [1..], x <= 1]
