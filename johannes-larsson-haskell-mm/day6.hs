import System.IO
import qualified Data.Set as Set (intersection, fromList)

splitLines :: String -> String -> [String]
splitLines r [] = [r]
splitLines r ('\n':'\n':ss) = r : splitLines "" ss
splitLines r (s:ss) = splitLines (r ++ [s]) ss

unique :: String -> String
unique [s] = [s]
unique (c:cs)
    | c `elem` cs = unique cs
    | otherwise = c : unique cs

remove :: Char -> String -> String
remove _ [] = []
remove r (c:cs)
    | r == c = remove r cs
    | otherwise = c : remove r cs

part1 :: String -> IO ()
part1 cont = do
    let groups = map (remove '\n') $ splitLines "" cont
    print "part 1:"
    print $ sum $ map (length . unique) groups

countGroup :: [String] -> Int
countGroup ss = length $ foldl Set.intersection (Set.fromList ['a'..'z']) (map Set.fromList ss)

part2 cont = do
    let groups = map lines $ splitLines "" cont
    print "part 2:"
    print $ sum $ map countGroup groups


main :: IO ()
main = do
    f <- openFile "day6" ReadMode
    cont <- hGetContents f
    part1 cont
    part2 cont