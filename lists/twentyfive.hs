import System.Random

rndPermu :: [a] -> IO [a]
rndPermu [] = do return []
rndPermu l = do
	g <- getStdGen
	let (xs, y:ys) = splitAt (fst $ randomR (0, length l - 1) g) l
	scrambledTail <- rndPermu (xs ++ ys)
	return $ y : scrambledTail
