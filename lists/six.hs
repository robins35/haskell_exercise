isPalindrome :: (Eq a, Ord a) => [a] -> Bool
isPalindrome xs = xs == reverse xs
