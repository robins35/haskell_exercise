myLast :: [a] -> a
myLast [a] = a
myLast (x:xs) = myLast xs
