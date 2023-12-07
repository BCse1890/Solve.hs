{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module M2Lecture9 where

import Control.Monad.Logger
import Control.Monad (foldM)
import Data.Bits
import Data.Char (ord)
import Data.ByteString (ByteString, unpack)
import qualified Data.List as L
import Data.Text (pack)
import Data.Word (Word8, Word64)

import Utils

member :: Int -> [Word8] -> Bool
member i bs = undefined

insert :: Int -> [Word8] -> [Word8]
insert i bs = undefined

delete :: Int -> [Word8] -> [Word8]
delete i bs = undefined

leftShift :: [Word8] -> [Word8]
leftShift bs = undefined

rightShift :: [Word8] -> [Word8]
rightShift bs = undefined

lonelyNumber :: [Word8] -> Int
lonelyNumber vals = undefined

decodeHuffman :: (MonadLogger m) => ByteString -> HuffmanTree -> m String
decodeHuffman input tree = undefined

ballDrop :: (MonadLogger m) => (Int, Int) -> [Int] -> [Int] -> [Int] -> [Int] -> m Bool
ballDrop (h, v) spikes1 spikes2 spikes3 spikes4 = undefined
