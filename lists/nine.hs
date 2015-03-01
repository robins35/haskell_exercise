pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs)
	| x == head xs = (x : (head (pack xs))) : (tail (pack xs))
	| otherwise = [x] : pack xs
