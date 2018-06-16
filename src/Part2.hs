module Part2 (Encode(Single, Multiple), problem11) where

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
