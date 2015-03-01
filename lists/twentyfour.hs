import System.Random

rndSelect :: [a] -> Int -> [a]
rndSelect l n = map (\i -> l !! i) $ take n $ randomRs (0, (length l)-1) (mkStdGen 345345345)

diffSelect :: Int -> Int -> [Int]
diffSelect n e = rndSelect [1..e] n
