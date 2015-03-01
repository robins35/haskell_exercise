import System.Random

rndPermu :: [a] -> IO [a]
rndPermu [] = return()
rndPermu [x] = return([x])
rndPermu l = do
	g <- getStdGen
	return $ let (ys, zs) = splitAt $ randomR (0, length l - 1) in rndPermu zs ++ rndPermu ys

