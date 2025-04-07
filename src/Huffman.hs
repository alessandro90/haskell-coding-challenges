module Huffman where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as PQ
import Data.Word (Word8)

-- data Tree = Node

countFreq :: ByteString -> MinPQueue Int Word8
countFreq bs =
  let m' = BS.foldr' (\b m -> M.insertWith (+) b 1 m) M.empty bs
   in M.foldrWithKey (flip PQ.insert) PQ.empty m'
