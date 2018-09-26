module Basics where

import Data.Function

-- sumSquares = (+) `on` (^ 2)
--
-- h = snd
--
-- g = (*)
-- multSecond = g `on` h
--
-- on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
-- on3 op fn x y z = op (fn x) (fn y) (fn z)
-- g :: b -> c
-- h :: a -> b
-- compose g h = \a -> g (h a)
h = max 42

g = (^ 3)

f = logBase 2

doItYourself = f . g . h

-- avg :: (Double, Double) -> Double
-- avg p = (fst p + snd p) / 2
-- class Eq a where
--   (==), (/=) :: a -> a -> Bool
class Printable p where
  toString :: p -> [Char]

instance Printable Bool where
  toString True = "true"
  toString False = "false"

instance Printable () where
  toString () = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
  toString p = "(" ++ toString (fst p) ++ "," ++ toString (snd p) ++ ")"

class KnownToGork a where
  stomp :: a -> a
  doesEnrageGork :: a -> Bool

class KnownToMork a where
  stab :: a -> a
  doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) =>
      KnownToGorkAndMork a
  where
  stompOrStab :: a -> a
  stompOrStab p
    | doesEnrageMork p && doesEnrageGork p = stomp (stab p)
    | doesEnrageMork p = stomp p
    | doesEnrageGork p = stab p
    | otherwise = p

ip = show a ++ show b ++ show c ++ show d
  where
    a = "127."
    b = "224."
    c = "120."
    d = "12"

-- class Enum a where
--   succ, pred :: a -> a
--   toEnum :: Int -> a
--   fromEnum :: a -> Int
class (Bounded a, Eq a, Enum a) =>
      SafeEnum a
  where
  ssucc :: a -> a
  ssucc p
    | p == maxBound = minBound
    | otherwise = succ p
  spred :: a -> a
  spred p
    | p == minBound = maxBound
    | otherwise = pred p

instance SafeEnum Bool

instance SafeEnum Char

avg :: Int -> Int -> Int -> Double
avg x y z = (fromInteger $ sum x y z) / 3
  where
    sum :: Int -> Int -> Int -> Integer
    sum a b c = toInteger a + toInteger b + toInteger c

at 0 (x : xs)
