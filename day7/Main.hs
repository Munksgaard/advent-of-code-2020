module Main where

import Data.Char
import Data.Function
import Data.List
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Set as S

type Rules = M.Map String (M.Map String Int)

lineToRule :: String -> Rules
lineToRule s =
  let [this, rest] = splitOn " bags contain " s
   in splitWhen (== ',') rest
        & foldMap (helper . words)
        & M.singleton this
  where
    helper [cnt, adj, color, _] = M.singleton (adj ++ " " ++ color) $ read cnt
    helper _ = mempty

canContainGold :: Rules -> String -> Bool
canContainGold rules s =
  let keys = M.keys $ rules M.! s
   in "shiny gold" `elem` keys
        || any (canContainGold rules) keys

bagsInBag :: Rules -> String -> Int
bagsInBag rules s =
  rules M.! s
    & M.mapWithKey ((*) . bagsInBag rules)
    & sum
    & (+ 1)

main :: IO ()
main = do
  rules <- foldMap lineToRule . lines <$> getContents
  print $ length $ filter (canContainGold rules) $ M.keys rules
  print $ pred $ bagsInBag rules "shiny gold"
