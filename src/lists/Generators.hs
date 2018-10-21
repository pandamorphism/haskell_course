module Generators where

coins = [2,3,7]
-- change :: (Ord a, Num a) => a -> [[a]]
change 0 = [[]]
change sum = [coin: res | coin <- coins, coin <= sum, res <- change (sum - coin)  ]
