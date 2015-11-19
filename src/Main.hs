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

  -- Ex 6.1
  snd :: (a, b) -> a
  snd (a, _) = a

  sing :: [a] -> a
  sing [x] = x

  shift :: ((a, b), c) -> (a, (b ,c))
  shift ((x, y), z) = (x, (y, z))

  -- Ex 6.5
  superImpose :: String -> String -> String
  superImpose [] [] = []
  superImpose (x : xs) (y : ys)
    | (x == '.') && (y == '.') = '.' : superImpose xs ys
    | otherwise = '#' : superImpose xs ys