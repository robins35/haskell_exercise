elementAt :: (Ord a, Integral b) => [a] -> b -> a
elementAt [] _ = error "Index is out of bounds"
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n - 1)
