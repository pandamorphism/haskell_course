module HighOrd where

import           Data.Char (isDigit, isUpper)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs)
  | p x = x : takeWhile' p xs
  | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p xs@(x:xs')
  | p x = dropWhile' p xs'
  | otherwise = xs

span' :: (a -> Bool) -> [a] -> ([a], [a])
span' p xs = (takeWhile' p xs, dropWhile' p xs)

break' :: (a -> Bool) -> [a] -> ([a], [a])
break' p = span' (not . p)

readDigits :: String -> (String, String)
readDigits xs = (takeWhile isDigit xs, dropWhile isDigit xs)

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj p1 p2 [] = []
filterDisj p1 p2 (x:xs)
  | p1 x || p2 x = x : filterDisj p1 p2 xs
  | otherwise = filterDisj p1 p2 xs

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = (qsort $ filter (< x) xs) ++ (x : (qsort $ filter (> x) xs))

map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map' f xs

concat' :: [[a]] -> [a]
concat' []       = []
concat' (xs:xss) = xs ++ concat' xss

concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f = concat' . map' f

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x ^ 2, x ^ 3])

perms :: Ord a => [a] -> [[a]]
perms [] = [[]]
perms (x:xs) =
  let len = length xs
      xperm p n =
        let (l, r) = splitAt n p
         in l ++ [x] ++ r
      xperms p = map (xperm p) [0 .. len]
   in concatMap xperms $ perms xs

and', or' :: [Bool] -> Bool
and' []     = True
and' (x:xs) = x && and' xs

or' []     = False
or' (x:xs) = x || or' xs

all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p

any' :: (a -> Bool) -> [a] -> Bool
any' p = or' . map p

rev = unwords . (map reverse) . words

delAllUpper = unwords . filterEmpty . map del . words
  where
    filterEmpty = filter (\x -> length x /= 0)
    del xs
      | all isUpper xs = ""
      | otherwise = xs

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _          = []
zipWith' _ _ []          = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 (\x y z -> max z (max x y))

fibStream = [0, 1] ++ zipWith (+) fibStream (tail fibStream)

repeat' :: a -> [a]
repeat' x = xs
  where
    xs = x : xs

replicate' :: Int -> a -> [a]
replicate' n x = take n $ repeat' x

cycle' :: [a] -> [a]
cycle' [] = error "cycle: empty list"
cycle' xs = ys
  where
    ys = xs ++ ys

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)

