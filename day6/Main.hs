module Main where

import Data.Char
import Data.Function
import Data.List
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = do
  input <- splitOn "\n\n" <$> getContents
  print $ sum $ fmap (length . nub . filter isAlpha) input
  print $ sum $ fmap (length . foldr intersect ['a' .. 'z'] . words) input
