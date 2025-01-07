import qualified Data.Map as M
import Data.Ord
part1 = do
    input <- readFile "input.txt"
    let (towels,patterns) = parseInput input
    print $ length $ filter (ispossible towels) patterns 
part2 = do
    input <- readFile "input.txt"
    let (towels,patterns) = parseInput input
    let xs = map (countpossible towels) patterns 
    print $ sum xs

ispossible::[String] -> String -> Bool
ispossible starts xs = go [xs] where 
    go xs = case xs of
        [] -> False
        ([]:xs) -> True
        (x:xs) -> go $ possibletails starts x ++ xs 

countpossible::[String] -> String -> Int
countpossible starts xs = go (M.singleton (LF xs) 1) where 
    go m = case M.maxViewWithKey m of
        Nothing -> 0
        Just ((LF "", n),rest) -> n
        Just ((LF xs,n),rest) -> go $ M.unionWith (+) rest $ M.fromList (zip (map LF $ possibletails starts xs) (repeat n))

newtype Longfirst = LF String deriving(Show, Eq)
instance Ord Longfirst where compare (LF a) (LF b) = (comparing length <> compare) a b
 
possibletails [] xs = []
possibletails (s:starts) xs
    |isprefix s xs = drop (length s) xs:possibletails starts xs
    |otherwise = possibletails starts xs

isprefix xs ys = take (length xs) ys == xs 

parseInput input = (towels,patterns) where
    (ts:space:ps) = lines input
    towels = words . filter (/= ',') $ ts
    patterns = ps 