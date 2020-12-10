module Main where

import Data.Char
import Data.Function
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Debug.Trace

part1 :: Int -> (Int, Int) -> [Int] -> (Int, Int)
part1 _ (diff1, diff3) [] = (diff1, diff3 + 1)
part1 current (diff1, diff3) (x : xs) =
  if current + 1 == x
    then part1 x (diff1 + 1, diff3) xs
    else
      if current + 3 == x
        then part1 x (diff1, diff3 + 1) xs
        else part1 x (diff1, diff3) xs

part2' :: M.Map (Int, [Int]) Int -> Int -> Int -> [Int] -> (Int, M.Map (Int, [Int]) Int)
part2' m current end [] =
  if current + 3 >= end then (1, m) else (0, m)
part2' m current end (x : xs) =
  case M.lookup (current, x : xs) m of
    Just res -> (res, m)
    Nothing ->
      if current + 3 >= x
        then
          let (tmp, m') = part2' m current end xs
              (tmp', m'') = part2' m' x end xs
           in (tmp + tmp', M.insert (current, (x : xs)) (tmp + tmp') m'')
        else (0, M.insert (current, (x : xs)) 0 m)

main :: IO ()
main = do
  ls <- sort . fmap read . lines <$> getContents
  print $ uncurry (*) $ part1 0 (0, 0) ls
  print $ fst $ part2' mempty 0 (last ls + 3) ls
