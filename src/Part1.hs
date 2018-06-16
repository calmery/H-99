module Part1 (problem1, problem2, problem3, problem4, problem5) where

problem1 :: [a] -> a
problem1 [x] = x
problem1 (x:xs) = problem1 xs

problem2 :: [a] -> a
problem2 (x:_:[]) = x
problem2 (_:xs) = problem2 xs

problem3 :: [a] -> Int -> a
problem3 (x:xs) 1 = x
problem3 (x:xs) n = problem3 xs (n - 1)

problem4 :: [a] -> Int
problem4 [_] = 1
problem4 (_:xs) = 1 + problem4 xs

problem5 :: [a] -> [a]
problem5 [] = []
problem5 (x:xs) = problem5 xs ++ [x]
