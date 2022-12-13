module Code where

data Tree a = Leaf a | Empty | Node (Tree a) a (Tree a)


flatten :: Tree t -> [t]
flatten Empty = []
flatten (Leaf n) = [n]
flatten (Node x y z) = flatten x ++ [y] ++ flatten z


insert :: Ord a => Tree a -> a -> Tree a
insert Empty x = Leaf x
insert (Leaf l) x = if x > l then Node (Leaf l) x Empty else Node (Leaf x) l Empty
insert (Node ll v rl) x = if x <= v then Node (insert ll x) v rl else Node ll v (insert rl x)