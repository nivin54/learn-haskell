{-# OPTIONS_GHC -Wall #-}

module Golf where

-- Hopscotch
-- using modulus, filter methods.
skip :: [n] -> [[n]]
skip n = concatMap (simple n) [1 .. (length n)]

simple :: [n] -> Int -> [[n]]
simple n sk = [wow n sk sk]

wow :: [a] -> Int -> Int -> [a]
wow [] _ _ = []
wow a sk bk =
  if (sk - 1) < length a
    then [a !! (sk - 1)] ++ (wow a (sk + bk) bk)
    else []

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [x] = []
localMaxima [x, y] = []
localMaxima (x:y:z:xs) =
  if (y > x) && (y > z)
    then y : localMaxima (z : xs)
    else localMaxima (y : z : xs)

-- histogram :: [Integer] -> String
-- histogram xs = concatMap (buildD xs) [1 .. (length xs)]
-- 
-- buildD :: [Integer] -> Int -> String
-- buildD xs len = concatMap (zzro xs len) [0 .. 9]
--   where
--     zzro xs len m =
--       if (length . (filter (\x -> x `mod` m) xs)) == len
--         then "*"
        else " "
