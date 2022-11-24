module Code where

sumsq n = foldr (\x y -> x^2 + y) 0 [0..n]