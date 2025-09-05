module Main where

import Data.Char
import Data.Function
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Debug.Trace
import Relude.List ((!!?))

parseInput1 :: String -> (Int, [Int])
parseInput1 s =
  case lines s of
    [earliest, busses] -> (read earliest, fmap read $ filter (/= "x") $ splitOn "," busses)
    _ -> undefined

doit :: (Int, Int) -> (Int, Int) -> (Int, Int)
doit (acc, base) (x, diff) =
  let new = fromJust $ elemIndex (x - diff) $ fmap (\i -> (i * acc + base) `mod` x) [0 ..]
   in (x * acc, new * acc + base)

main :: IO ()
main = do
  input <- getContents
  let (earliest, busses) = parseInput1 input
  print $ uncurry (*) $ minimumBy (compare `on` snd) $ fmap (\bus -> (bus, ((earliest `quot` bus) + 1) * bus - earliest)) busses
  lines input
    & tail
    & head
    & splitOn ","
    & zip [0 ..]
    & mapMaybe (\(i, x) -> case x of "x" -> Nothing; _ -> Just (read x :: Int, i))
    & foldl1 doit
    & print
