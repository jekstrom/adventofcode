import System.Environment
import System.IO  
import Control.Monad

-- input
-- 5 1 9 5
-- 7 5 3
-- 2 4 6 8
-- output
-- 18

-- [5,1,9,5] -> 8
difference :: (Foldable t, Ord a, Num a) => t a -> a
difference xs = maximum xs - minimum xs

-- [[5,1,9,5],[7,5,3]] -> 12
sums :: (Foldable t, Ord a, Num a) => [t a] -> a
sums [] = 0
sums (x:xs) = difference x + sums xs

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

readInt :: String -> Integer
readInt = read

mapInner :: (a -> b) -> [[a]] -> [[b]]
mapInner = map . map

parseInput :: String -> [[Integer]]
parseInput s = mapInner (readInt) $ map words (wordsWhen (=='\n') s)

main :: IO()
main = do
    args <- getArgs
    input <- return (args !! 0)
    contents <- readFile input
    let parsedInput = parseInput contents
    let answer = sums parsedInput
    print answer