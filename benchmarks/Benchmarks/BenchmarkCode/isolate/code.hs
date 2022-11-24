module Code where


isolate :: (Eq a) => [a] -> a -> ([a], [a])
isolate xs e = (isNotIn, isIn)
    where
        isIn = [x | x <- xs, x == e]
        isNotIn = [x | x <- xs, x /= e]
