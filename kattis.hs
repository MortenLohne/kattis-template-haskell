import Control.Monad
import Data.Maybe
import Data.List

import Data.Int
import Data.Word
import Data.Bits

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

readInt :: B.ByteString -> Int
readInt = fst . fromJust . C.readInt

readIntsFromLine :: B.ByteString -> [Int]
readIntsFromLine input =
  case C.readInt input of
    Just (n, input') -> if C.null input' then [n] else n:readIntsFromLine (C.tail input')
    Nothing -> []

-----------

-- reads an int from a line of stdin
getInt :: IO Int
getInt = fmap (fst . fromJust . C.readInt) B.getLine

-- reads space-separated ints from a line of stdin
getInts :: IO [Int]
getInts = fmap readIntsFromLine B.getLine

-- reads space-separated elements from a line of stdin
getReads :: Read a => IO [a] 
getReads = fmap ((map read) . words) getLine

pack :: (Int32, Int32) -> Word64
pack (x, y) = ((fromIntegral (fromIntegral x :: Word32) :: Word64) `shiftL` 32) .|. (fromIntegral (fromIntegral y :: Word32) :: Word64)

unpack :: Word64 -> (Int32, Int32)
unpack i =
  let yMask = 2^(32 :: Word64) - 1
      xMask = yMask `shiftL` 32
  in (fromIntegral $ (i .&. xMask) `shiftR` 32, fromIntegral $ i .&. yMask)

foldM' :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ z [] = return z
foldM' f z (x:xs) = do
  z' <- f z x
  z' `seq` foldM' f z' xs

main :: IO ()
main = undefined
