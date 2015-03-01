insertAt :: a -> [a] -> Int -> [a]
insertAt x l n = front ++ x : rest
	where (front, rest) = splitAt (n-1) l
