combinations :: Int -> [a] -> [[a]]
combinations n l = getCombis [0..n-1]
	where
		nextIndices [] _ = []
		nextIndices indices n
			| last indices == n-1 = let partIndices = nextIndices (init indices) (n-1)
				in if partIndices == [] then [] else partIndices ++ [last partIndices + 1]
			| otherwise = init indices ++ [last indices + 1]
		getCombis [] = []
		getCombis indices = combi indices : (getCombis $ nextIndices indices n)
		combi indices = map (l !!) indices

nextIndices :: [Int] -> Int -> [Int]
nextIndices [] _ = []
nextIndices indices n
	| last indices == n-1 = let partIndices = nextIndices (init indices) (n-1)
		in if partIndices == [] then [] else partIndices ++ [last partIndices + 1]
	| otherwise = init indices ++ [last indices + 1]

getCombis :: [Int] -> [[a]]
getCombis [] _ = []
getCombis indices n = combi indices : (getCombis $ nextIndices indices n)

combi :: [Int] -> [a]
combi indices = let l = "abcdef" in map (l !!) indices
