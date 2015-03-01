import System.Random

rndSelect :: [a] -> Int -> IO [a]
rndSelect l n = do
	g <- getStdGen
	return(map (\i -> l !! i) $ take n $ randomRs (0, (length l) - 1) g)

diffSelect :: Int -> Int -> IO [Int]
diffSelect res_len rng = rndSelect [1..rng] res_len
