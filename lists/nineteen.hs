rotate :: [a] -> Int -> [a]
rotate l n
	| n > length l = error "Index out of range"
	| n > 0 = rotSplit n
	| otherwise = rotSplit (length l + n)
	where rotSplit i = (\(first, second) -> second ++ first) $ splitAt i l
