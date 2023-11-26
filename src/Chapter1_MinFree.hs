module Chapter1_MinFree (main) where

import Control.Monad (when)
import Data.Array.IO (IOArray,newArray,writeArray,readArray)
import Data.List (sortBy, (\\), partition)
import Data.Ord (comparing)
import GHC.Int (Int64)
import System.Clock (TimeSpec(..),getTime,Clock(Monotonic))
import System.Random (randomRIO)
import Text.Printf (printf)

main :: IO ()
main = do
  putStrLn "*minfree*"
  putStrLn "naive..."
  exhibit naive_minfree
  putStrLn "array..."
  exhibit array_minfree
  putStrLn "divide..."
  exhibit array_divideAndConquer

naive_minfree :: [Int] -> IO Int
naive_minfree xs = do
  pure $ head ([0..] \\ xs)

array_minfree :: [Int] -> IO Int
array_minfree xs = do
  let size = length xs
  a :: IOArray Int Bool <- newArray (0,size) False
  sequence_ [ writeArray a x True | x <- xs, x < size ]
  final <- sequence [ do b <- readArray a x; pure (x,b) | x <- [0..size] ]
  let unmarked = [ x | (x,b) <- final, not b ]
  pure $ head unmarked

array_divideAndConquer :: [Int] -> IO Int
array_divideAndConquer xs0 = do
  let
    minFrom :: Int -> (Int,[Int]) -> Int
    minFrom a (n,xs) = do
      if (n == 0) then a else do
        let b = a+1 + n `div` 2
        let (ys,zs) = partition (< b) xs
        let ny = length ys
        if (ny == b - a)
          then minFrom b (n - ny, zs)
          else minFrom a (ny, ys)
  pure $ minFrom 0 (length xs0, xs0)

exhibit :: ([Int] -> IO Int) -> IO ()
exhibit f = loop 0
  where
    limit = Nanos (300_000_000) -- 0.3 sec
    loop :: Int -> IO ()
    loop n = do
      -- takes far longer to create the problem instance
      inp <- makeProblemInstance n
      let !_ = sum inp -- ensure the instance is fully created.
      (nanos,res) <- timed (do !res <- f inp; pure res)
      printf "[%s] instance, n/res=%d\n" (show nanos) (check n res)
      when (nanos < limit) $ loop (2*n+1)

makeProblemInstance :: Int -> IO [Int]
makeProblemInstance n = do -- where n is the desired result
  shuffle ([0..n-1] ++ [ n+2*i+1 | i <- [0..n-1] ]) --2*n elements; n is missing

shuffle :: [a] -> IO [a]
shuffle xs = do
  ds <- genRand (length xs)
  pure $ map snd (sortBy (comparing fst) (zip ds xs))

genRand :: Int -> IO [Double]
genRand n = sequence $ replicate n $ randomRIO (0,1)

check :: (Eq a, Show a) => a -> a -> a
check a b = if a == b then a else error ("check failed: " ++ show a ++ " not same as: " ++ show b)

timed :: IO a -> IO (Nanos,a)
timed io = do
  before <- getTime Monotonic
  res <- io
  after <- getTime Monotonic
  let TimeSpec{sec,nsec} = after - before
  let time = Nanos (gig * sec + nsec)
  pure (time,res)

newtype Nanos = Nanos Int64 deriving (Eq,Ord,Num)

instance Show Nanos where
  show (Nanos i) = printf "%.03fs" dub
    where dub :: Double = fromIntegral i / fromIntegral gig

gig :: Int64
gig = 1_000_000_000
