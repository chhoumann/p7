module Code where


alldots :: Num a => [(a, a)] -> [(a, a)] -> [a]
alldots as bs = [a * c + b * d |
    (a, b) <- as,
    (c, d) <- bs
    ]