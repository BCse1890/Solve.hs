{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module M2Lecture10 where

import Control.Monad
import Control.Monad.Logger
import qualified Data.Aeson as Ae
import qualified Data.Aeson.KeyMap as Ae
import Data.Bits
import qualified Data.ByteString as B
import Data.Char (chr)
import Data.Function (on)
import Data.Int (Int32)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Ord as O
import Data.Scientific
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import Data.Word (Word8)
import Utils
import qualified Data.Text as T

data Command =
  LoadValue String Int32 |
  -- First register is destination
  -- e.g. AddR "A" "B" ==> "A" += "B"
  AddR String String |
  SubR String String |
  MultR String String |
  AndR String String |
  OrR String String |
  XorR String String |
  RShiftR String Int |
  LShiftR String Int |
  PrintR String

upgradedAssembly :: (MonadLogger m) => [Command] -> m [Int32]
upgradedAssembly commands = undefined

pick2Clumps :: (MonadLogger m) => [Int] -> m Int
pick2Clumps values = undefined

budgetCalculation :: (MonadLogger m) => Ae.Value -> m Scientific
budgetCalculation value = undefined

encodeHuffman :: (MonadLogger m) => HuffmanTree -> String -> m B.ByteString
encodeHuffman tree input = undefined

skylineArea :: (MonadLogger m) => [(Int, Int, Int)] -> m Int
skylineArea buildings = undefined

intervalAddition :: (MonadLogger m) => [((Int,Int),Int)] -> m Int
intervalAddition queries = undefined
