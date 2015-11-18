module Main where

  main :: IO()
  main = do
    putStrLn "Hello!"
    putStrLn (show (fourEqual 1 2 3 4))

  mystery :: Integer -> Integer -> Integer -> Bool
  mystery m n p = not ((m==n) && (p==n))

  threeEqual :: Int -> Int -> Int -> Bool
  threeEqual a b c
    = (a == b) && (b == c) && (a == c)

  threeDifferent :: Integer -> Integer -> Integer -> Bool
  threeDifferent a b c
    = a /= b && b /= c && a /= c

  fourEqual :: Int -> Int -> Int -> Int -> Bool
  fourEqual a b c d
    = (a == b) && (b == c) && (c == d)


  fourEqual1 :: Int -> Int -> Int -> Int -> Bool
  fourEqual1 a b c d
    = (threeEqual a b c) && (threeEqual  b c d)