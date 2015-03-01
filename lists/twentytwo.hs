range :: Int -> Int -> [Int]
range s e
	| s > e = []
	| otherwise = s : range (s+1) e
