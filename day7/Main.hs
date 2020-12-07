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
  helper $ words s
  where
    helper [adj, color, "bags", "contain", "no", "other", "bags."] =
      M.singleton (adj ++ " " ++ color) mempty
    helper [adj, color, "bags", "contain", cnt, adj', color', _] =
      M.singleton (adj ++ " " ++ color) $ M.singleton (adj' ++ " " ++ color') $ read cnt
    helper [adj, color, "bags", "contain", cnt, adj', color', _, cnt', adj'', color'', _] =
      M.singleton (adj ++ " " ++ color) $
        M.fromList
          [ (adj' ++ " " ++ color', read cnt),
            (adj'' ++ " " ++ color'', read cnt')
          ]
    helper (adj : color : "bags" : "contain" : rest) =
      M.singleton (adj ++ " " ++ color)
        $ foldMap (helper' . words)
        $ splitWhen (== ',')
        $ unwords rest
    helper _ = undefined
    helper' [cnt, adj, color, _] = M.singleton (adj ++ " " ++ color) $ read cnt
    helper' _ = undefined

canContainGold :: Rules -> [String] -> String -> Bool
canContainGold rules alreadyChecked s =
  if s `elem` alreadyChecked
    then False
    else case M.lookup s rules of
      Just m ->
        any (== "shiny gold") (M.keys m)
          || any (canContainGold rules (s : alreadyChecked)) (M.keys m)

bagsInBag :: Rules -> [String] -> String -> Int
bagsInBag rules alreadyChecked s =
  if s `elem` alreadyChecked
    then 0
    else case M.lookup s rules of
      Just m ->
        M.mapWithKey ((*) . bagsInBag rules (s : alreadyChecked)) m
          & sum
          & (+ 1)
      Nothing ->
        undefined

main :: IO ()
main = do
  rules <- foldMap lineToRule <$> lines <$> getContents
  print $ length $ filter (canContainGold rules []) $ M.keys rules
  print $ pred $ bagsInBag rules [] "shiny gold"
