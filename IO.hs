{-|
Author: Tomas Möre
Year: 2019

Warning, the IO defined here is not entirely compatible with the standard
haskell prints and reads as it fully ignores the buffering Haskell does behind
the scenes.

For printing out large amounts of text i would instead sugest usiong the
'ByteString.Builder' library together with its 'hPutBuilder' function.  In those
cases it can be good to enable some output buffering to lower the ammount of
syscalls and any attempt to output anything else then ASCII.

See System.IO:
> hSetBuffering stdout (BlockBuffering (Just 1000000))
> hSetEncoding stdout char8

For convienience the language extention OverloadedStrings can be used to syntactically write contant ByteStrings or ByteString-Builders directly.

-}
module IO where
import Prelude hiding (getChar)
import Data.Word
import Data.Int
import Data.ByteString.Internal (w2c)

import qualified Data.Vector.Unboxed (Unbox)

import Control.Monad.Primitive
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

-- | Input reading class, warning, inherently unsafe... but fast!
class Input a where
  -- | Reads input disregarding possible whitespace before the element
  nextInput :: IO a
  -- | Retrieves one input element, no
  getInput :: IO a

instance Input Int32 where
  nextInput = nextInt32
  getInput = getInt32

instance Input Int64 where
  nextInput = nextInt64
  getInput = getInt64

instance Input Int where
  nextInput = fromIntegral <$> nextInt64
  getInput = fromIntegral <$> getInt64

-- | No difference between nextInput and getInput for pure bytes
instance Input Word8 where
  nextInput = getByte
  getInput = getByte

instance (Input a, Input b) => Input (a, b) where
  nextInput = (,) <$> nextInput <*> nextInput
  getInput = (,) <$> getInput <*> nextInput

instance (Input a, Input b, Input c) => Input (a, b, c) where
  nextInput = (,,) <$> nextInput <*> nextInput <*> nextInput
  getInput = (,,) <$> getInput <*> nextInput <*> nextInput

instance (Input a, Input b, Input c, Input d) => Input (a, b, c, d) where
  nextInput = (,,,) <$> nextInput <*> nextInput <*> nextInput <*> nextInput
  getInput = (,,,) <$> getInput <*> nextInput <*> nextInput <*> nextInput

foreign import ccall "get_char" c_getChar :: IO Word8

nextVectorM :: (MV.Unbox a, Input a) => Int -> IO (MV.MVector (PrimState IO) a)
nextVectorM size = do
  MV.replicateM size nextInput

nextVector :: (MV.Unbox a, Input a) => Int -> IO (V.Vector a)
nextVector size = do
  v <- MV.replicateM size nextInput
  V.unsafeFreeze v

getString :: Int -> IO (V.Vector Char)
getString size = do
  v <- MV.replicateM size getChar
  V.unsafeFreeze v

-- | gets one byte directly fro
getByte :: IO Word8
getByte = c_getChar

-- | Gets one character from input, only reads ASCII encoded characters
getChar :: IO Char
getChar = w2c <$> c_getChar

-- | Skips one character from input
skipChar :: IO ()
skipChar = getByte >> pure ()


foreign import ccall "next_char" c_nextChar :: IO Word8

-- | Reads one character from input, will prefix disregard whisteses
nextChar :: IO Char
nextChar = w2c <$> c_nextChar

foreign import ccall "get_int64" getInt64 :: IO Int64
foreign import ccall "get_int32" getInt32 :: IO Int32

foreign import ccall "next_int64" nextInt64 :: IO Int64
foreign import ccall "next_int32" nextInt32 :: IO Int32

nextInt :: IO Int
nextInt = fromIntegral <$> nextInt64

foreign import ccall "next_word32" nextWord32 :: IO Word32
foreign import ccall "next_word64" nextWord64 :: IO Word64

foreign import ccall "println_int64" printInt64Ln :: Int64 -> IO ()
foreign import ccall "println_int32" printInt32Ln :: Int32 -> IO ()

printIntLn :: Int -> IO ()
printIntLn = printInt64Ln . fromIntegral

foreign import ccall "print_spaced_int64" printInt64Space :: Int64 -> IO ()
foreign import ccall "print_spaced_int32" printInt32Space :: Int32 -> IO ()

foreign import ccall "print_space" printSpace :: IO ()
foreign import ccall "print_newline" printNewline :: IO ()
