removeAt :: Int -> [a] -> (a, [a])
removeAt n l
	| not $ n `elem` [1..(length l)] = error "Index out of range"
	| otherwise = (elm, first ++ rest)
	where (first, elm:rest) = splitAt (n-1) l
