import System.Environment

-- [1,1,1,2,2] -> [(1,1),(1,1),(1,2),(2,2),(2,1)]
shift xs = tail xs ++ [head xs]

loop xs = zip xs $ shift xs

digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

main :: IO()
main = do
    args <- getArgs
    input <- return (args !! 0)
    let num = read input :: Integer
    let groups = loop $ digs num
    let answer = foldl (\acc (x, y) -> if x == y then x + acc else acc) 0 groups
    putStrLn $ show answer