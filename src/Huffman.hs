module Huffman where

import Control.Monad (when)
import Control.Monad.ST (runST)
import Data.Bifunctor (Bifunctor (first, second))
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.Functor (void, (<&>))
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as PQ
import Data.STRef (modifySTRef, newSTRef, readSTRef)
import Data.Word (Word32, Word8)
import Misc (bsTraverseBits)

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

codes :: Tree Word8 -> M.Map Word8 [Bit]
codes = go M.empty []
  where
    go m bits (Leaf (_, c)) = M.insert c (reverse bits) m
    go m bits (Node _ l r) =
      let m1 = go m (BitOff : bits) l
          m2 = go m (BitOn : bits) r
       in M.union m1 m2

encode :: M.Map Word8 [Bit] -> ByteString -> Either Word8 (ByteString, Word32)
encode m bs =
  let encoded = BS.foldl' accumulateBytes (Right (bfEmpty, 0)) bs
   in encoded <&> first bfAllBytes
  where
    accumulateBytes accumulator w = do
      (bitbuffer, c) <- accumulator
      case M.lookup w m of
        Nothing -> Left w
        Just bits -> Right (foldl' bfPush bitbuffer bits, succ c)

data DecodeState
  = DecodeState
  { originalTree :: Tree Word8,
    originalByteCount :: Word32,
    currentTree :: Tree Word8,
    currentByteCount :: Word32
  }

-- check out Binary package, in particular the BitGet monad

-- TODO: implement with State monad
decode :: Tree Word8 -> Word32 -> ByteString -> Maybe ByteString
decode fullTree totalCodes encoded = runST $ do
  subtreeRef <- newSTRef fullTree
  parsedCodesRef <- newSTRef totalCodes
  decodedRef <- newSTRef $ Just BS.empty
  let forEachBit bit = do
        parsedCodes <- readSTRef parsedCodesRef
        when (parsedCodes < totalCodes) $ do
          subtree <- readSTRef subtreeRef
          case subtree of
            Node _ l r ->
              let subtree' = if bit then l else r
               in case subtree' of
                    Leaf (_, b) -> do
                      modifySTRef decodedRef $ fmap $ BS.cons b
                      modifySTRef parsedCodesRef succ
                      modifySTRef subtreeRef $ const fullTree
                    node -> modifySTRef subtreeRef $ const node
            Leaf _ -> do
              -- this is an error. We should never arrive here
              modifySTRef decodedRef $ const Nothing
              modifySTRef parsedCodesRef $ const totalCodes
  bsTraverseBits forEachBit encoded
  fmap BS.reverse <$> readSTRef decodedRef

data BitBuffer = BitBuffer
  { bytes :: BS.ByteString,
    byte :: Word8,
    byteIndex :: Int
  }
  deriving (Show, Eq)

data Bit = BitOn | BitOff deriving (Show, Eq, Ord)

bfEmpty :: BitBuffer
bfEmpty = BitBuffer {bytes = BS.empty, byte = 0, byteIndex = 0}

bfPush :: BitBuffer -> Bit -> BitBuffer
bfPush bf@BitBuffer {bytes, byte, byteIndex} b
  | byteIndex == 7 =
      let byte' = if b == BitOn then byte .|. 1 `shiftL` 7 else byte
          bytes'' = byte' `BS.cons` bytes
       in BitBuffer {byte = 0, bytes = bytes'', byteIndex = 0}
  | otherwise = case b of
      BitOn ->
        let byte' = byte .|. 1 `shiftL` byteIndex
         in bf {byte = byte', byteIndex = byteIndex + 1}
      BitOff -> bf {byteIndex = byteIndex + 1}

bfAllBytes :: BitBuffer -> BS.ByteString
bfAllBytes bf
  | byteIndex bf == 0 = BS.reverse $ bytes bf
  | otherwise = BS.reverse (bytes bf) `BS.append` BS.singleton (byte bf)
