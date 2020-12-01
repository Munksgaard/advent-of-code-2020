module Main where

main :: IO ()
main = do
  input <- getContents
  let lns = read <$> lines input
  let res1 = [x * y | x <- lns, y <- lns, x + y == 2020]
  let res2 = [x * y * z | x <- lns, y <- lns, z <- lns, x + y + z == 2020]
  print $ head res1
  print $ head res2
