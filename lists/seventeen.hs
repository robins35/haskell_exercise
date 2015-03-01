split :: [a] -> Int -> ([a], [a])
split [] _ = error "Index out of range"
split l 0 = ([], l)
split (x:xs) n = (x : before, after)
	where (before, after) = split xs (n-1)
