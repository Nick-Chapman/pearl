module Chapter1_MinFree (main) where

import GHC.Int (Int64)
import Data.List (sortBy, (\\))
import Data.Ord (comparing)
import System.Random (randomRIO)
import Text.Printf (printf)
import System.Clock (TimeSpec(..),getTime,Clock(Monotonic))

main :: IO ()
main = do
  putStrLn "*minfree*"
  exhibit minfree

-- naive quadratic algorithm
minfree :: [Int] -> Int
minfree xs = head ([0..] \\ xs)

exhibit :: ([Int] -> Int) -> IO ()
exhibit f = loop 1
  where
    loop :: Int -> IO ()
    loop n = do
      inp <- makeProblemInstance n
      (nanos,res) <- timed (do let !res = f inp
                               pure res)
      printf "[%s] instance, n/res=%d\n" (show nanos) (check n res)
      loop (2*n)

makeProblemInstance :: Int -> IO [Int]
makeProblemInstance n = do -- where n is the desired result
  shuffle ([0..n-1] ++ [n+1..2*n]) --2*n elements; n is missing

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
