import Control.Monad
import Data.Maybe
import Data.List

import Data.Int
import Data.Word
import Data.Bits

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Data.Array.IO

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

get :: (Ix i) => IOArray i e -> i -> IO e
get = readArray

put :: (Ix i) => IOArray i e -> i -> e -> IO ()
put = writeArray

mapIndex  :: (Ix i) => IOArray i e -> i -> (e -> e) -> IO ()
mapIndex arr i f = fmap f (readArray arr i) >>= writeArray arr i

type Node = Int64
type CountingTree = IOUArray Int Node

nearestPower :: Int -> Int
nearestPower n = n .&. (-n)

-- Updates all values smaller than and including idx (1-indexed)
update :: CountingTree -> Int -> Node -> IO()
update _ 0 _ = return ()
update tree idx v = do
  oldVal <- readArray tree idx
  writeArray tree idx (oldVal + v)
  update tree (idx - (nearestPower idx)) v

-- Query a specific index (1-indexed)
query :: CountingTree -> Int -> Int -> IO Node
query tree' size idx' =
  let query' :: CountingTree -> Int -> Node -> IO Node
      query' tree idx result = 
        if idx > size then return result
        else do
          val <- readArray tree idx
          query' tree (idx + (nearestPower idx)) (val + result)
  in query' tree' idx' 0

printTree :: CountingTree -> Int -> IO ()
printTree tree n = forM [1..n] (query tree n) >>= print

-- Find between low (inclusive) and high (inclusive)
-- If value not found, returns largest index with value smaller than target
binarySearch :: (Int -> IO Ordering) -> Int -> Int -> IO Int
binarySearch f low high =
    if high - low <= 1
    then do
      ord <- f low
      ord' <- f high
      case (ord, ord') of
       (_, LT) -> return low
       (EQ, EQ) -> return low
       (GT, _) -> return high
       _ -> undefined
    else do
      let mid = low + (high - low) `div` 2
      ord <- f mid
      case ord of
       LT -> binarySearch f low mid
       EQ -> return mid
       GT -> binarySearch f mid high

-- Find between low (inclusive) and high (inclusive)
-- If value not found, returns a nearby index
binarySearchExact :: (Int -> IO Ordering) -> Int -> Int -> IO Int
binarySearchExact f low high =
    if high - low <= 1
    then fmap (\ord -> if ord == EQ then high else low) (f high)
    else do
      let mid = low + (high - low) `div` 2
      ord <- f mid
      case ord of
       LT -> binarySearchExact f low (mid - 1)
       EQ -> return mid
       GT -> binarySearchExact f (mid + 1) high

binarySearchArray :: Ord a => IOArray Int a -> a -> IO Int
binarySearchArray arr v = do
  (low, high) <- getBounds arr
  binarySearch (\i -> fmap (compare v) (readArray arr i)) low high

main :: IO ()
main = undefined
