module Top where

import Chapter1_MinFree as Ch1 (main)

main :: IO ()
main = do
  putStrLn "*pearl*"
  Ch1.main
