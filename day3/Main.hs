module Main where

countTrees :: [[Char]] -> Int -> Int -> Int
countTrees hill right down =
  helper (drop down hill) right
  where
    helper :: [[Char]] -> Int -> Int
    helper [] _ = 0
    helper xs@(current : _) idx = do
      (if current !! idx == '#' then 1 else 0)
        + helper (drop down xs) (idx + right)

main :: IO ()
main = do
  input <- getContents
  let hill = fmap cycle $ lines input
  print $ countTrees hill 3 1
  print $
    product
      [ countTrees hill 1 1,
        countTrees hill 3 1,
        countTrees hill 5 1,
        countTrees hill 7 1,
        countTrees hill 1 2
      ]
