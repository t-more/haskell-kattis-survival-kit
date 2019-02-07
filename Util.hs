{-# LANGUAGE Strict #-}
module Util (
  bsToWord
  , bsToInt
            ) where

import qualified Data.ByteString as BSRaw
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Internal as BS (c2w)
import Data.Word

-- | Parses a bytestring as an
bsToWord :: BSRaw.ByteString -> Word
bsToWord =
  BSRaw.foldl' (\ a w ->
                   10 * a + (fromIntegral w - (fromIntegral (BS.c2w '0')))
               ) 0

bsToInt :: BSRaw.ByteString -> Int
bsToInt s =
  if BSRaw.head s == BS.c2w '-'
  then negate (fromIntegral $ bsToWord (BSRaw.tail s))
  else fromIntegral $ bsToWord s
