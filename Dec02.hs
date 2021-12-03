import System.FilePath
import Data.Char

--formatInput :: String -> [[String]]

calculateHor :: FilePath -> IO Int
calculateHor path =
    do file <- readFile path
       let lineFormat = lines file
       let horOnly = filter (\x -> length x == 9) lineFormat
       let numbers = map (digitToInt . last) horOnly
       return $ sum numbers

calculateVer :: FilePath -> IO Int
calculateVer path =
    do file <- readFile path
       let lineFormat = lines file
       let upOnly = filter (\x -> length x == 4) lineFormat
       let upNumbers = map (digitToInt . last) upOnly
       let downOnly = filter (\x -> length x == 6) lineFormat
       let downNumbers = map (digitToInt . last) downOnly
       return $ sum upNumbers - sum downNumbers

total :: FilePath -> IO Int
total path =
    do file <- readFile path
       let lineFormat = lines file
       let horOnly = filter (\x -> length x == 9) lineFormat
       let horNumbers = map (digitToInt . last) horOnly
       let upOnly = filter (\x -> length x == 4) lineFormat
       let upNumbers = map (digitToInt . last) upOnly
       let downOnly = filter (\x -> length x == 6) lineFormat
       let downNumbers = map (digitToInt . last) downOnly
       return $ sum horNumbers * (sum downNumbers - sum upNumbers)