module Code where


pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple n = [(a, b, c) |
    c <- [1..n],
    b <- [0..c-1],
    a <- [0..b],
    a <= b,
    b < c,
    a^2 + b^2 == c^2
    ]