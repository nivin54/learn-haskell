module Solution_hw1 where

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = (toDigits (n `div` 10)) ++ [(n `mod` 10)]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = [(n `mod` 10)] ++ (toDigitsRev (n `div` 10))

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:(y:zs)) = [x, (y * 2)] ++ doubleEveryOther zs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (x `div` 10) + (x `mod` 10) + sumDigits xs

validate :: Integer -> Bool
validate x = mod (sumDigits (doubleEveryOther  (toDigitsRev x))) 10 == 0
