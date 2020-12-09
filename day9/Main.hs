module Main where

import Data.Char
import Data.Function
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Debug.Trace

invalidNum :: [Int] -> Int
invalidNum acc =
  case [x + y | x <- take 25 acc, y <- take 25 acc, x + y == acc !! 25] of
    [] -> acc !! 25
    _ -> invalidNum $ tail acc

part2 :: [Int] -> [Int] -> Int -> Int
part2 xs acc target =
  case compare (sum acc) target of
    EQ -> minimum acc + maximum acc
    LT -> part2 (tail xs) (head xs : acc) target
    GT -> part2 xs (init acc) target

main :: IO ()
main = do
  ls <- fmap read . lines <$> getContents
  let n = invalidNum ls
  print n
  print $ part2 ls [] n
