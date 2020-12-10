import System.IO

l = 25

readInt :: String -> Int
readInt = read

canSumTo :: Int -> [Int] -> Bool
canSumTo _ [] = False
canSumTo n (m:ms) = elem (n-m) ms || canSumTo n ms

check :: [Int] -> Bool
check ns = canSumTo (ns !! l) (take l ns)

part1 ns
    | length ns < (l+1) = -1
    | check ns = part1 $ drop 1 ns
    | otherwise = ns !! l

main = do
    f <- openFile "day9" ReadMode
    cont <- hGetContents f
    let ns = map readInt $ lines cont
    print "part 1:"
    print $ part1 ns