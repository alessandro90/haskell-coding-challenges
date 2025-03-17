module Misc where

startsWith :: (Eq a) => [a] -> [a] -> Bool
startsWith [] _ = True
startsWith (_ : _) [] = False
startsWith (p : ps) (x : xs) = p == x && startsWith ps xs

startsWithItem :: (Eq a) => a -> [a] -> Bool
startsWithItem _ [] = False
startsWithItem c (x : _) = c == x
