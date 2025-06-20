module Misc where

import Control.Monad.ST (ST)
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
w8ToBinaryString = w8FoldBits (\s bit -> s <> (if bit then "1" else "0")) ""

bsToBinaryString :: BS.ByteString -> String
bsToBinaryString = bsFoldBits (\s bit -> s <> (if bit then "1" else "0")) ""

bsFoldBits :: (a -> Bool -> a) -> a -> BS.ByteString -> a
bsFoldBits = BS.foldl' . w8FoldBits

w8FoldBits :: (a -> Bool -> a) -> a -> Word8 -> a
w8FoldBits f acc w = go 0 acc
  where
    go idx acc'
      | idx == 7 = f acc' (getBit 7)
      | otherwise = go (succ idx) $ f acc' $ getBit idx
    getBit idx = w .&. (1 `shiftL` idx) /= 0

bsToBits :: BS.ByteString -> [Bool]
bsToBits = reverse . bsFoldBits (flip (:)) []

bsTraverseBits :: (Bool -> ST s ()) -> BS.ByteString -> ST s ()
bsTraverseBits f = BS.foldl' (\_ w -> w8TraverseBits f w) $ pure ()

w8TraverseBits :: (Monad m) => (Bool -> m a) -> Word8 -> m a
w8TraverseBits f w = go 0
  where
    go idx
      | idx == 7 = f $ getBit 7
      | otherwise = f (getBit idx) >> go (succ idx)
    getBit idx = w .&. (1 `shiftL` idx) /= 0

splitStr :: Char -> String -> [String]
splitStr c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : splitStr c s''
    where
      (w, s'') = break (== c) s'

splitStrWhen :: (Char -> Bool) -> String -> [String]
splitStrWhen f s = case dropWhile f s of
  "" -> []
  s' -> w : splitStrWhen f s''
    where
      (w, s'') = break f s'
