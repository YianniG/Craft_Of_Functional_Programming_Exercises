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

  ----------------------
  -- Exploratory work --
  ----------------------
  type Value = Int
  type Loc = (Int, Int)
                                  -- L     R     U     D
  data Pixel = Dead | Live Value Loc Pixel Pixel Pixel Pixel
              deriving(Show)

  px00, px01, px1, px2, px3, px4 :: Pixel
  
  px00 = Live 0 (0,0) Dead px01 Dead Dead
  px01 = Live 1 (0,1) px00 px02 px03 Dead
  px02 = Live 2 (0,2) px01 Dead Dead Dead
  px03 = Live 3 (0,3) Dead Dead Dead px01

  px1 = Live 1 (0,0) Dead px2 px4 Dead
  px2 = Live 2 (0,1) px1 Dead px3 Dead
  px3 = Live 3 (1,1) px4 Dead Dead px2
  px4 = Live 4 (1,0) Dead px3 Dead px1

  -- Need accumulator to manage where I've been.
  traversePixels :: Pixel -> [Int]
  traversePixels Dead = []
  traversePixels (Live v _ Dead Dead Dead Dead) = [v]
  traversePixels (Live v _ p1@(Live v1 l1 p11 p12 p13 p14)
                             p2@(Live v2 l2 p21 p22 p23 p24)
                             p3@(Live v3 l3 p31 p32 p33 p34)
                             p4@(Live v4 l4 p41 p42 p43 p44))
      = traversePixels (Live v1 l1 p11 Dead p13 p14) ++
        traversePixels (Live v2 l2 Dead p22 p23 p24) ++
        traversePixels (Live v3 l3 p31 p32 p33 Dead) ++
        traversePixels (Live v4 l4 p41 p42 Dead p44) ++ [v]

  traversePixels (Live v _ Dead
                             p2@(Live v2 l2 p21 p22 p23 p24)
                             p3@(Live v3 l3 p31 p32 p33 p34)
                             p4@(Live v4 l4 p41 p42 p43 p44))
    = traversePixels (Live v2 l2 Dead p22 p23 p24) ++
      traversePixels (Live v3 l3 p31 p32 p33 Dead) ++
      traversePixels (Live v4 l4 p41 p42 Dead p44) ++ [v]

  traversePixels (Live v _ p1@(Live v1 l1 p11 p12 p13 p14)
                               Dead
                               p3@(Live v3 l3 p31 p32 p33 p34)
                               p4@(Live v4 l4 p41 p42 p43 p44))
    = traversePixels (Live v1 l1 p11 Dead p13 p14) ++
      traversePixels (Live v3 l3 p31 p32 p33 Dead) ++
      traversePixels (Live v4 l4 p41 p42 Dead p44) ++ [v]

  traversePixels (Live v _ p1@(Live v1 l1 p11 p12 p13 p14)
                               p2@(Live v2 l2 p21 p22 p23 p24)
                               Dead
                               p4@(Live v4 l4 p41 p42 p43 p44))
    = traversePixels (Live v1 l1 p11 Dead p13 p14) ++
      traversePixels (Live v2 l2 Dead p22 p23 p24) ++
      traversePixels (Live v4 l4 p41 p42 Dead p44) ++ [v]

  traversePixels (Live v _ p1@(Live v1 l1 p11 p12 p13 p14)
                               p2@(Live v2 l2 p21 p22 p23 p24)
                               p3@(Live v3 l3 p31 p32 p33 p34)
                               Dead)
    = traversePixels (Live v1 l1 p11 Dead p13 p14) ++
      traversePixels (Live v2 l2 Dead p22 p23 p24) ++
      traversePixels (Live v3 l3 p31 p32 p33 Dead) ++ [v]

  traversePixels (Live v _ Dead
                             Dead
                             p3@(Live v3 l3 p31 p32 p33 p34)
                             p4@(Live v4 l4 p41 p42 p43 p44))

    = traversePixels (Live v3 l3 p31 p32 p33 Dead) ++
      traversePixels (Live v4 l4 p41 p42 Dead p44) ++ [v]

  traversePixels (Live v _ Dead
                               p2@(Live v2 l2 p21 p22 p23 p24)
                               Dead
                               p4@(Live v4 l4 p41 p42 p43 p44))
    = traversePixels (Live v2 l2 Dead p22 p23 p24) ++
      traversePixels (Live v4 l4 p41 p42 Dead p44) ++ [v]

  traversePixels (Live v _ Dead
                               p2@(Live v2 l2 p21 p22 p23 p24)
                               p3@(Live v3 l3 p31 p32 p33 p34)
                               Dead)
    = traversePixels (Live v2 l2 Dead p22 p23 p24) ++
      traversePixels (Live v3 l3 p31 p32 p33 Dead) ++ [v]

  traversePixels (Live v _ p1@(Live v1 l1 p11 p12 p13 p14)
                               Dead
                               Dead
                               p4@(Live v4 l4 p41 p42 p43 p44))
    = traversePixels (Live v1 l1 p11 Dead p13 p14) ++
      traversePixels (Live v4 l4 p41 p42 Dead p44) ++ [v]

  traversePixels (Live v _ p1@(Live v1 l1 p11 p12 p13 p14)
                               Dead
                               p3@(Live v3 l3 p31 p32 p33 p34)
                               Dead)
    = traversePixels (Live v1 l1 p11 Dead p13 p14) ++
      traversePixels (Live v3 l3 p31 p32 p33 Dead) ++ [v]

  traversePixels (Live v _ p1@(Live v1 l1 p11 p12 p13 p14)
                             p2@(Live v2 l2 p21 p22 p23 p24)
                             Dead
                             Dead)
    = traversePixels (Live v1 l1 p11 Dead p13 p14) ++
      traversePixels (Live v2 l2 Dead p22 p23 p24) ++ [v]

  traversePixels (Live v _ Dead
                             Dead
                             Dead
                             p4@(Live v4 l4 p41 p42 p43 p44))
    = traversePixels (Live v4 l4 p41 p42 Dead p44) ++ [v]

  traversePixels (Live v _ Dead
                             Dead
                             p3@(Live v3 l3 p31 p32 p33 p34)
                             Dead)
    = traversePixels (Live v3 l3 p31 p32 p33 Dead)  ++ [v]

  traversePixels (Live v _ Dead
                             p2@(Live v2 l2 p21 p22 p23 p24)
                             Dead
                             Dead)
    = traversePixels (Live v2 l2 Dead p22 p23 p24) ++ [v]

  traversePixels (Live v _ p1@(Live v1 l1 p11 p12 p13 p14)
                             Dead
                             Dead
                             Dead)
      = traversePixels (Live v1 l1 p11 Dead p13 p14) ++ [v]

{-

  traversePixels (Live v _ p1@(Live v1 l1 p11 p12 p13 p14)
                                p2@(Live v2 l2 p21 p22 p23 p24)
                                p3@(Live v3 l3 p31 p32 p33 p34)
                                p4@(Live v4 l4 p41 p42 p43 p44))
          = traversePixels (Live v1 l1 p11 Dead p13 p14) ++
            traversePixels (Live v2 l2 Dead p22 p23 p24) ++
            traversePixels (Live v3 l3 p31 p32 p33 Dead) ++
            traversePixels (Live v4 l4 p41 p42 Dead p44)

    traversePixels (Live v _ Dead
                              p2@(Live v2 l2 p21 p22 p23 p24)
                              p3@(Live v3 l3 p31 p32 p33 p34)
                              p4@(Live v4 l4 p41 p42 p43 p44))
        = traversePixels (Live v2 l2 Dead p22 p23 p24) ++
          traversePixels (Live Dead l3 p31 p32 p33 Dead) ++
          traversePixels (Live Dead l4 p41 p42 Dead p44)

    traversePixels (Live v _ p1@(Live v1 l1 p11 p12 p13 p14)
                              Dead
                              p3@(Live v3 l3 p31 p32 p33 p34)
                              p4@(Live v4 l4 p41 p42 p43 p44))
        = traversePixels (Live v1 l1 p11 Dead p13 p14) ++
          traversePixels (Live v3 l3 p31 Dead p33 Dead) ++
          traversePixels (Live v4 l4 p41 Dead Dead p44)

    traversePixels (Live v _ p1@(Live v1 l1 p11 p12 p13 p14)
                              p2@(Live v2 l2 p21 p22 p23 p24)
                              Dead
                              p4@(Live v4 l4 p41 p42 p43 p44))
        = traversePixels (Live v1 l1 p11 Dead Dead p14) ++
          traversePixels (Live v2 l2 Dead p22 Dead p24) ++
          traversePixels (Live v4 l4 p41 p42 Dead p44)

    traversePixels (Live v _ p1@(Live v1 l1 p11 p12 p13 p14)
                              p2@(Live v2 l2 p21 p22 p23 p24)
                              p3@(Live v3 l3 p31 p32 p33 p34)
                              Dead)
        = traversePixels (Live v1 l1 p11 Dead p13 Dead) ++
          traversePixels (Live v2 l2 Dead p22 p23 Dead) ++
          traversePixels (Live v3 l3 p31 p32 p33 Dead)
-}
