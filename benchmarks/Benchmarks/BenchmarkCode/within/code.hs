module Code where

within :: (Ord a, Eq a) => [a] -> (a, a) -> [a]
within xs (min, max) = filter (\x -> x >= min && x <= max) xs