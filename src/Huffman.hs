module Huffman where

import Data.Bifunctor (Bifunctor (second))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Map.Strict as M
import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as PQ
import Data.Word (Word8)

data Tree a
  = Node Int (Tree a) (Tree a)
  | Leaf (Int, a)
  deriving (Eq, Show)

leaf :: Int -> Char -> Tree Word8
leaf n c = Leaf (n, BSI.c2w c)

instance (Eq a) => Ord (Tree a) where
  (Leaf (f, _)) `compare` (Leaf (f', _)) = f `compare` f'
  (Leaf (f, _)) `compare` (Node n' _ _) = f `compare` n'
  (Node n _ _) `compare` (Leaf (n', _)) = n `compare` n'
  (Node n _ _) `compare` (Node n' _ _) = n `compare` n'

instance Semigroup (Tree a) where
  ll@(Leaf (n, _)) <> ll'@(Leaf (n', _)) = Node (n + n') ll ll'
  ll@(Leaf (n, _)) <> nn@(Node n' _ _) = Node (n + n') ll nn
  nn@(Node n _ _) <> nn'@(Node n' _ _) = Node (n + n') nn nn'
  nn@(Node n _ _) <> ll@(Leaf (n', _)) = Node (n + n') nn ll

instance Functor Tree where
  fmap f (Leaf v) = Leaf $ second f v
  fmap f (Node v l r) = Node v (fmap f l) (fmap f r)

countFreq :: ByteString -> MinQueue (Tree Word8)
countFreq bs =
  let m' = BS.foldr' (\b m -> M.insertWith (+) b 1 m) M.empty bs
   in M.foldrWithKey (\k v q -> PQ.insert (Leaf (v, k)) q) PQ.empty m'

buildTree :: (Eq a) => MinQueue (Tree a) -> Maybe (Tree a)
buildTree queue = PQ.minView queue >>= go
  where
    go (t, q) = case PQ.minView q of
      Nothing -> Just t
      Just (t', q') -> buildTree $ PQ.insert (t <> t') q'
