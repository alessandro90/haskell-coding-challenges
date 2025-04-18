module Misc where

import Data.Bits (shiftL, (.&.))
import qualified Data.ByteString as BS
import Data.Word (Word8)

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

w8ToBinaryString :: Word8 -> String
w8ToBinaryString = go 0 []
  where
    go offset acc w
      | offset == 7 = bitAnd w offset : acc
      | otherwise = go (offset + 1) (bitAnd w offset : acc) w
    bitAnd w offset = if w .&. (1 `shiftL` offset) == 0 then '0' else '1'

bsToBinaryString :: BS.ByteString -> String
bsToBinaryString = BS.foldl' (\acc w -> w8ToBinaryString w <> acc) ""
