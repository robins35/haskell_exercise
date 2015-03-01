dropEvery :: [a] -> Int -> [a]
dropEvery l n = let
	dropHelper [] _ = []
	dropHelper (x:xs) i
		| i `mod` n == 0 = dropHelper xs (i + 1)
		| otherwise = x : dropHelper xs (i + 1)
	in dropHelper l 1
