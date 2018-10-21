module Lists where

import           Prelude hiding (init, last, length, null, (++))

addTwoElements :: a -> a -> [a] -> [a]
addTwoElements a b list = a : b : list

nTimes :: a -> Int -> [a]
nTimes a b
  | b == 0 = []
  | otherwise = inner a b []
  where
    inner x 0 acc       = acc
    inner x counter acc = inner x (counter - 1) (x : acc)

second :: [a] -> a
second = head . tail

second' :: [a] -> a
second' (x:xs) = head xs

second'' :: [a] -> a
second'' (_:x:_) = x

length :: [a] -> Int
length []     = 0
length (x:xs) = 1 + length xs

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : xs ++ ys

null :: [a] -> Bool
null [] = True
null _  = False

oddsOnly :: Integral a => [a] -> [a]
oddsOnly xs
  | xs == [] = []
  | otherwise = runner xs []
  where
    runner [] odds = reverse odds
    runner (x:xs) odds =
      runner
        xs
        (if odd x
           then x : odds
           else odds)

oddsOnly' :: Integral a => [a] -> [a]
oddsOnly' [] = []
oddsOnly' (x:xs)
  | odd x = x : oddsOnly' xs
  | otherwise = oddsOnly' xs

last :: [a] -> a
last (x:[]) = x
last (_:xs) = last xs

init :: [a] -> [a]
init [_]    = []
init (x:xs) = x : init xs

sum' :: (Num a) => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

reverse' :: [a] -> [a]
reverse' l = rev l []
  where
    rev [] a     = a
    rev (x:xs) a = rev xs (x : a)

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = run xs (reverse xs) True
  where
    run [] [] res         = res
    run (x:xs) (y:ys) res = run xs ys (res && x == y)

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _          = []
zip' as []         = []
zip' (a:as) (b:bs) = (a, b) : zip' as bs

unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' ((x, y):xys) =
  let (xs, ys) = unzip' xys
   in (x : xs, y : ys)

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 (x:xs) (y:ys) (z:zs) = (x + y + z) : sum3 xs ys zs
sum3 (x:xs) (y:ys) _      = (x + y) : sum3 xs ys []
sum3 _ (y:ys) (z:zs)      = (y + z) : sum3 [] ys zs
sum3 (x:xs) _ (z:zs)      = (x + z) : sum3 xs [] zs
sum3 (x:xs) _ _           = x : sum3 xs [] []
sum3 _ (y:ys) _           = y : sum3 [] ys []
sum3 _ _ (z:zs)           = z : sum3 [] [] zs
sum3 _ _ _                = []

groupElems :: Eq a => [a] -> [[a]]
groupElems = group []
  where
    group a [] = reverse a
    group [] (x:xs) = group [[x]] xs
    group ((y:ys):yss) (x:xs)
      | x == y = group ((x : y : ys): yss) xs
      | otherwise = group ([x] : (y : ys) : yss) xs

take' :: Int -> [a] -> [a]
take' n _       | n <= 0 = []
take' _ []      = []
take' n (x: xs) = x : take' (n-1) xs

drop' :: Int -> [a] -> [a]
drop' n xs      | n <= 0 = xs
drop' n []      = []
drop' n (x: xs) = drop (n-1) xs


splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs = (take n xs, drop n xs)


