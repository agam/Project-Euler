{-# OPTIONS_GHC -Wall -Werror #-}
module Main where

-- Current Project Euler max-project-number
max_problem_num :: Integer
max_problem_num = 1

-- Problem 1 - Add all the natural numbers below one thousand that are multiples of 3 or 5.
problem1 :: Integer
problem1 = sum (filter (\x -> (x `mod` 3 == 0 || x `mod` 5 == 0)) [1 .. 999])

-- Add new problems to this list as they are created
eulerEval :: Integer -> Integer
eulerEval 1 = problem1
eulerEval _ = 0

-- | main, do Nothing
main :: IO()
main = mapM_ print (map eulerEval [1 .. max_problem_num])
