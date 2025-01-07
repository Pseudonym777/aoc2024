import Data.Function
import Data.Char
import Text.Read
import Data.Maybe
import Data.List
import qualified Data.Map as M
import Data.Monoid
main::IO ()
main = do
    input <- readFile "input.txt"
    print $ part1 input 
    
part1 input = sum . map sum $ map ((map (count "XMAS")) . ($ lines input)) transformations
part2 input = sum . map fromEnum $ (\x -> map (isMiddle x) (M.keys x)) (coordMap $ lines input)

d::[[a]] -> [a]
d [] = []
d ([]:xs) = []
d ((x:xs):ys) = x:d (map (drop 1) ys)

diagonals::[[a]] -> [[a]]
diagonals xs = dsLeft xs ++ (drop 1 $ dsLeft (transpose xs))
dsLeft xs = [d lefts| lefts <- tails xs]

powerset [] = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)

transformations = map (foldl (.) id) $ powerset [map reverse, diagonals, transpose . reverse]

count xs [] = 0
count xs (y:ys) = if take (length xs) (y:ys) == xs then 1+ count xs ys else count xs ys


coordMap::[[a]] -> M.Map (Int,Int) a 
coordMap xs = M.fromList . concat .  map (zipWith (\a (b,x) ->((b,a),x)) [0..]) . transpose . map (zip [0..]) $ xs

isMiddle:: M.Map (Int,Int) Char ->  (Int,Int) -> Bool
isMiddle m (x,y) = Just True == do
    ul <- M.lookup (x-1,y+1) m
    ur <- M.lookup (x+1,y+1) m
    dl <- M.lookup (x-1,y-1) m
    dr <- M.lookup (x+1,y-1) m
    c  <- M.lookup (x,y) m
    return $ and [c == 'A',sort [ul,dr] == "MS",sort [ur,dl] == "MS"]







