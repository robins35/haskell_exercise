myReverse :: (Ord a) => [a] -> [a]
myReverse xs = foldr (\x acc ->  acc ++ [x]) [] xs
