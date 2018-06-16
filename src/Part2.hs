module Part2 (Encode(Single, Multiple), problem11, problem12) where

import Part1

-- Problem 11
data Encode a = Single a | Multiple Int a
  deriving (Eq, Show)

problem11 :: Eq a => [a] -> [Encode a]
problem11 xs = map helper $ problem10 xs
  where
    helper :: (Int, a) -> Encode a
    helper (1, e) = Single e
    helper (n, e) = Multiple n e

problem12 :: [Encode a] -> [a]
problem12 xs = helper' $ map helper xs
  where
    helper :: Encode a -> [a]
    helper (Single x) = [x]
    helper (Multiple n x) = take n $ repeat x
    helper' :: [[a]] -> [a]
    helper' (xs:[]) = xs
    helper' (xs:xxs) = xs ++ helper' xxs
