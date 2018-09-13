module Intro where

import Data.Char

discount :: Double -> Double -> Double -> Double
discount limit proc sum =
  if sum >= limit
    then sum * (100 - proc) / 100
    else sum

standardDiscount :: Double -> Double
standardDiscount = discount 1000 5

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y =
  if isDigit x && isDigit y
    then digitToInt x * 10 + digitToInt y
    else 100

dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt ((fst p2 - fst p1) ^ 2 + (snd p2 - snd p1) ^ 2)

doubleFact :: Integer -> Integer
doubleFact 0 = 1
doubleFact (-1) = 1
doubleFact n = n * doubleFact (n - 2)

factorial 0 = 1
factorial n =
  if n < 0
    then error "arg must be >= 0"
    else n * factorial (n - 1)

factorial' 0 = 1
factorial' n
  | n < 0 = error "arg must be >= 0"
  | n > 0 = n * factorial' (n - 1)

factorial'' n
  | n >= 0 = helper 1 n
  | otherwise = error "arg must be >= 0"

helper acc 0 = acc
helper acc n = helper (acc * n) (n - 1)

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci (-1) = 1
fibonacci n
  | n > 0 = fibonacci (n - 1) + fibonacci (n - 2)
  | n < 0 = fibonacci (n + 2) - fibonacci (n + 1)

fib n
  | n == 0 = 0
  | otherwise =
    let runner prev next 0 = prev
        runner prev next n
          | n > 0 = runner next (prev + next) (n - 1)
          | n < 0 = runner next (prev - next) (n + 1)
     in runner 0 1 n

seqA :: Integer -> Integer
seqA k =
  let runner x y acc 0 = x
      runner x y acc k
        | k > 0 = runner y acc (y + acc - 2 * x) (k - 1)
        | otherwise = error "Argument should be gte 0"
   in runner 1 2 3 k

-- Using 'where' clause
sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x
  | x == 0 = (0, 1)
  | otherwise = runner (abs x) 0 0
  where
    runner 0 sum count = (sum, count)
    runner m sum count = runner (div m 10) (sum + mod m 10) (count + 1)

-- Using 'let' clause
sumAndCount :: Integer -> (Integer, Integer)
sumAndCount x
  | x == 0 = (0, 1)
  | otherwise =
    let runner 0 sum count = (sum, count)
        runner m sum count = runner (div m 10) (sum + mod m 10) (count + 1)
     in runner (abs x) 0 0

integration :: (Double -> Double) -> Double -> Double -> Double
integration fn a b = doIt fn a 1000 ((b - a) / 1000) 0.0
  where
    doIt fn from iter step acc
      | iter == 0 = acc
      | otherwise =
        let to = from + step
         in doIt fn to (iter - 1) step (acc + 0.5 * step * (fn from + fn to))


