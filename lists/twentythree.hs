import System.Random

rndSelect :: [a] -> Int -> [a]
rndSelect l n = map (\i -> l !! i) $ take n $ randomRs (0, (n-1)) (mkStdGen 48)
