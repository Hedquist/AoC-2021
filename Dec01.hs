import System.FilePath

test :: IO ()
test = putStr "Hello World\n"

depthIncreaseAmount :: FilePath -> IO Int 
depthIncreaseAmount f = 
    do file <- readFile f
       let linedInts = map read $ lines file :: [Int]
       return $ dea $ windows linedInts
       
dea :: [Int] -> Int
dea [_]      = 0
dea (x:y:zs) | x < y     = 1 + dea (y:zs)
             | otherwise = dea $ y:zs

windows :: [Int] -> [Int]
windows (x:y:z:as) = (x + y + z) : windows (y : z : as)
windows _          = []