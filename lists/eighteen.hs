slice :: [a] -> Int -> Int -> [a]
slice l s e = let
	range = [s..e]
	sliceHelper [] _ = []
	sliceHelper (x:xs) i
		| i `elem` range = x : sliceHelper xs (i+1)
		| otherwise = sliceHelper xs (i+1)
	in sliceHelper l 1
