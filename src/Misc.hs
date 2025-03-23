module Misc where

startsWith :: (Eq a) => [a] -> [a] -> Bool
startsWith [] _ = True
startsWith (_ : _) [] = False
startsWith (p : ps) (x : xs) = p == x && startsWith ps xs

startsWithItem :: (Eq a) => a -> [a] -> Bool
startsWithItem _ [] = False
startsWithItem c (x : _) = c == x

digitsNr :: Int -> Int
digitsNr n
  | n `div` 10 == 0 = 1
  | otherwise = 1 + digitsNr (n `mod` 10)
