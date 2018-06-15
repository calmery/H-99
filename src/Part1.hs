module Part1 (problem1) where

problem1 :: [a] -> a
problem1 [x] = x
problem1 (x:xs) = problem1 xs
