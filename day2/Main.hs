module Main where

import Data.List

type Rule = (Int, Int, Char, String)

lineToRule :: String -> Rule
lineToRule s =
  let [restrs, letter', passwd] = words s
      letter = init letter'
      (from', to') = break ((==) '-') restrs
   in (read from', read $ tail to', head letter, passwd)

validPassword :: Rule -> Bool
validPassword (from, to, c, passwd) =
  let count = length $ filter (== c) passwd
   in from <= count && count <= to

validPassword2 :: Rule -> Bool
validPassword2 (from, to, c, passwd) =
  passwd !! (from - 1) == c && not (passwd !! (to - 1) == c)
    || not (passwd !! (from - 1) == c) && passwd !! (to - 1) == c

main :: IO ()
main = do
  input <- getContents
  let rules = lineToRule <$> lines input
  print $ length $ filter validPassword rules
  print $ length $ filter validPassword2 rules
