module PolymorphismP6 where

data Trivial =
  Trivial

instance Eq Trivial where
  Trivial == Trivial = True

data DayOfWeek
  = Mon
  | Tue
  | Wed
  | Thu
  | Fri
  | Sat
  | Sun
  deriving (Show)

data Date =
  Date DayOfWeek
       Int
  deriving (Show)

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _     = False

instance Eq Date where
  (==) (Date weekday dayOfMonth) (Date weekday' dayOfMonth') =
    weekday == weekday' && dayOfMonth == dayOfMonth'

data TisAnInteger = TisAn Integer deriving (Show)

instance Eq TisAnInteger where
  (==) (TisAn a) (TisAn b) = a == b

data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
 (==) (Two a b) (Two a' b') = a == a' && b == b'

data StringOrInt = TisAnInt Int | TisAString String
instance Eq StringOrInt where
 (==) (TisAnInt a) (TisAnInt a')     = a == a'
 (==) (TisAString b) (TisAString b') = b == b'
 (==) _ _                            = False

data Pair a = Pair a a
instance Eq a => Eq (Pair a) where
 (==) (Pair a b) (Pair a' b') = a == a' && b == b'

data Tuple a b = Tuple a b
instance (Eq a, Eq b) =>  Eq (Tuple a b) where
 (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

data Which a = ThisOne a | ThatOne a
instance Eq a => Eq (Which a) where
 (==) (ThisOne a) (ThisOne a') = a == a'
 (==) (ThatOne a) (ThatOne a') = a == a'
 (==) _ _                      = False

data EitherOr a b = Hello a | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
 (==) (Hello a) (Hello a')     = a == a'
 (==) (Goodbye a) (Goodbye a') = a == a'
 (==) _ _                      = False
