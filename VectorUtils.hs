{-
Author: Tomas Möre
Year: 2019
-}

module VectorUtils  ( quicksort, repliacteWIdxM, quicksortCmp, makePii, piiFst, piiSnd, PII) where

import Control.Monad.ST
import Control.Monad
import Control.Monad.Primitive

import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GMV
import Data.Vector.Unboxed (Unbox(..), Vector, MVector)
import qualified Data.Vector.Unboxed.Mutable as MV
import IO
import Data.Word
import Data.Bits
import Data.Int
import Text.Printf

-- | Haskell unboxed vectors creates two diffrent vectors for pairs by default.
-- therefore i creaded this ugly little hack to works with 32 bit pairs!
type PII = Int64

piiSnd b = b .&. 0x00000000FFFFFFFF
piiFst a = shiftR a 32
makePii a b = shiftL a 32 .|. b

showPii p = printf "(%d, %d)" (piiFst p) (piiSnd p)

-- | Constructs a vector in place. Contents of the vector will be the return
-- value of the monadi action
repliacteWIdxM :: (PrimMonad m, Unbox a) => Int -> (Int -> m a) -> m (MV.MVector (PrimState m) a)
repliacteWIdxM size initF = do
  arr <- MV.unsafeNew size
  let init i =
        if i < size
        then do MV.unsafeWrite arr i =<< (initF i)
                init (i + 1)
        else pure ()
  init 0
  pure arr

-- | Type synonyum for the quciksort
type Comparator a = a -> a -> Bool


-- | Mutable quicksort that defaults to using the standard ord (<) method for the
-- contents
quicksort :: (Ord a, PrimMonad m, Unbox a) => MV.MVector (PrimState m) a -> m ()
quicksort v = do
  quicksort' v (<) 0 (MV.length v - 1)

-- | Mutable quicksort where you supply your own comparison operator.
quicksortCmp :: (PrimMonad m, Unbox a) => MV.MVector (PrimState m) a -> Comparator a -> m ()
quicksortCmp v lessThen = do
  quicksort' v lessThen 0 (MV.length v - 1)

quicksort' :: (PrimMonad m, Unbox a) => MV.MVector (PrimState m) a -> Comparator a -> Int -> Int -> m ()
quicksort' v lessThen low high =
  if low >= high
  then return ()
  else do
    pivot <- MV.read v ((low + high) `div` 2)
    mid <- hoarePartition v lessThen low high pivot
    quicksort' v lessThen low mid
    quicksort' v lessThen (mid + 1) high


hoarePartition :: (PrimMonad m, Unbox a) => MV.MVector (PrimState m) a -> Comparator a -> Int -> Int -> a -> m Int
hoarePartition v lessThen low high pivot = loop (low - 1) (high + 1)
  where
    nextLow prev = do
      let i = prev + 1
      e <- MV.unsafeRead v i
      if e `lessThen` pivot
        then nextLow i
        else return i

    nextHigh base = do
      let j = base - 1
      e <- MV.unsafeRead v j
      if pivot `lessThen` e
         then nextHigh j
         else return j

    loop prevLow prevHigh = do
      i <- nextLow prevLow
      j <- nextHigh prevHigh
      if i >= j
        then return j
        else MV.unsafeSwap v i j >> loop i j
