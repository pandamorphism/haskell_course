module RFold where

sumList :: [Integer] -> Integer
sumList []     = 0
sumList (x:xs) = x + sumList xs

productList :: [Integer] -> Integer
productList []     = 1
productList (x:xs) = x * productList (xs)

concatList :: [[a]] -> [a]
concatList []     = []
concatList (x:xs) = x ++ concatList xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f ini []     = ini
foldr' f ini (x:xs) = x `f` foldr f ini xs

sList :: [Integer] -> Integer
sList = foldr' (+) 0

sConcatList :: [[a]] -> [a]
sConcatList = foldr' (++) []

lengthList :: [a] -> Int
lengthList = foldr' (\_ s -> 1 + s) 0

sumOdd :: [Integer] -> Integer
sumOdd =
  foldr
    (\x s ->
       if odd x
         then x + s
         else s)
    0

-- foldl' :: (b -> a -> b) -> b -> [a] -> b
-- foldl' f ini []      = ini
-- foldl' f ini (x: xs) = foldl' f (f ini x) xs
{-
foldl f ini 1:2:3:[]
~> foldl f (f ini 1) (2 : 3 : [])
~> foldl f ( f ( f ini 1) 2) (3 : [])
~> foldl f ( f ( f ini 1) 2) 3 ) []
~> f ( f (f ini 1) 2) 3
-}
meanList :: [Double] -> Double
meanList = last . foldr (\x [sum, step, average] -> [sum + x, step + 1, (sum + x) / step]) [0,1,0]

evenOnly :: [a] -> [a]
evenOnly xs = let l = length xs in
    fst $ foldr (\x (xs, step) -> if even step then (x: xs, step - 1) else (xs, step + 1)) ([], l) xs

lastElem :: [a] -> a
lastElem = foldl1 const . reverse