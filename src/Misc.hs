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

bsTraverseBits :: (Monoid a, Monad m) => (Bool -> m a) -> BS.ByteString -> m a
bsTraverseBits f = BS.foldl' (\_ w -> w8TraverseBits f w) $ pure mempty

w8TraverseBits :: (Monad m) => (Bool -> m a) -> Word8 -> m a
w8TraverseBits f w = go 0
  where
    go idx
      | idx == 7 = f $ getBit 7
      | otherwise = f (getBit idx) >> go (succ idx)
    getBit idx = w .&. (1 `shiftL` idx) /= 0

-- w8TraverseBits' :: (Monad m) => (a -> Bool -> m a) -> a -> Word8 -> m a
-- w8TraverseBits' f acc w = go 0 acc
--   where
--     go idx acc'
--       | idx == 7 = f acc' $ getBit 7
--       | otherwise = do
--           ma <- f acc' (getBit idx)
--           go (succ idx) ma
--     getBit idx = w .&. (1 `shiftL` idx) /= 0
--
-- bsTraverseBits' :: (Monad m) => (a -> Bool -> m a) -> a -> BS.ByteString -> m a
-- bsTraverseBits' f acc bs =
--   BS.foldl'
--     ( \ma w -> do
--         ma' <- ma
--         w8TraverseBits' f ma' w
--     )
--     acc
--     bs
