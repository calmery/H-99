module Part1 (problem1, problem2, problem3, problem4, problem5, problem6, NestedList(Elem, List), problem7, problem8) where

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

problem6 :: Eq a => [a] -> Bool
problem6 xs = xs == problem5 xs

-- Problem 7
data NestedList a = Elem a | List [NestedList a]

problem7 :: NestedList a -> [a]
problem7 (Elem x) = [x]
problem7 (List []) = []
problem7 (List (x:xs)) = problem7 x ++ problem7 (List xs)

problem8 :: Eq a => [a] -> [a]
problem8 (x:[]) = [x]
problem8 (x:y:xs) = if x == y then problem8 (x:xs) else [x] ++ problem8 (y:xs)
