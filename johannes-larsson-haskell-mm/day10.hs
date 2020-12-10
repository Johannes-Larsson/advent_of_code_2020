import System.IO
import Data.List (sort)

readInt :: String -> Int
readInt = read

countDiffs :: [Int] -> [Int]
countDiffs [c] = [0,0]
countDiffs (c1:c2:cs)
    | c2 - c1 == 1 = zipWith (+) [1, 0] $ countDiffs (c2:cs)
    | c2 - c1 == 3 = zipWith (+) [0, 1] $ countDiffs (c2:cs)
    | otherwise = [-1, c1,c2]

main = do
    f <- openFile "day10" ReadMode
    cont <- hGetContents f
    let as = map readInt (lines cont)
    print "part 1:"
    print $ product $ countDiffs $ sort (0 : 3 + maximum as : as)