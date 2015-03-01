pack :: (Ord a, Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs)
	| x == head xs = (x : head (pack xs)) : (tail (pack xs))
	| otherwise = [x] : (pack xs)

pack' :: (Ord a, Eq a) => [a] -> [[a]]
pack' [] = []
pack' (x:xs) = (x : takeWhile (== x) xs) : pack' (dropWhile (== x) xs)

encode :: (Ord a, Eq a, Show a) => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . pack

data EncodedElem a = Multiple Int a | Single a deriving Show


encodeModified :: (Ord a, Eq a, Show a) => [a] -> [EncodedElem a]
encodeModified = map (\x -> if length x > 1
					then Multiple (length x) (head x)
					else Single (head x))
					. pack

decodeModified :: (Ord a, Show a) => [EncodedElem a] -> [a]
decodeModified [] = []
decodeModified (Single x : xs) = x : decodeModified xs
decodeModified (Multiple n x : xs) = replicate n x ++ decodeModified xs

encodeDirect :: (Ord a, Eq a, Show a) => [a] -> [EncodedElem a]
encodeDirect [] = []
encodeDirect (x:xs)
	| x == head xs = Multiple (length reps) x : encodeDirect rest
	| otherwise = Single x : encodeDirect xs
	where (reps, rest) = (x : takeWhile (== x) xs, dropWhile (== x) xs)
