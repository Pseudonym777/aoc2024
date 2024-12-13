import Data.Function
import Data.Char
import Text.Read
import Data.Maybe
import Data.List
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Set as S

main::IO ()
main = do
	input <- readFile "input.txt"
	let guardmap = coordMap $ lines $ input
	print $ part2 guardmap
part1 = length . guardPathList
findstart::M.Map (Int,Int) Char -> (Int,Int)
findstart m = fst $ head $ M.toList $ M.filter (flip elem "><^v") m

finddir m = case snd $ head $ M.toList $ M.filter (flip elem "><^v") m of
	'^' -> U
	'>' -> R
	'v' -> D
	'<' -> L

coordMap::[[a]] -> M.Map (Int,Int) a 
coordMap xs = M.fromList . concat . transpose .   map (zipWith (\a (b,x) ->((b,a),x)) [0..]) . transpose . map (zip [0..]) $(reverse xs)

data Direction = U | R | D | L deriving(Enum, Eq, Ord, Show)

newdir L = U
newdir x = succ x

newpos (x,y) U = (x,y+1)
newpos (x,y) D = (x,y-1)
newpos (x,y) R = (x+1,y)
newpos (x,y) L = (x-1,y)

guardPath::M.Map (Int,Int) Char -> (Int,Int) -> Direction -> S.Set ((Int,Int),Direction) -> S.Set ((Int,Int),Direction)
guardPath m pos direction history
  |S.member (pos,direction) history = history
  |otherwise = case M.lookup (newpos pos direction) m of
	Nothing -> S.insert (pos,direction) history
	Just '#' -> guardPath m pos (newdir direction) history
	Just openspace -> guardPath m (newpos pos direction) direction (S.insert (pos,direction) history)

guardPathList m = S.toList $ S.map fst $ guardPath m (findstart m) (finddir m) S.empty 

guardPathLoop::M.Map (Int,Int) Char -> (Int,Int) -> Direction -> S.Set ((Int,Int),Direction) -> Bool
guardPathLoop m pos direction history
  |S.member (pos,direction) history = True
  |otherwise = case M.lookup (newpos pos direction) m of
	Nothing -> False
	Just '#' -> guardPathLoop m pos (newdir direction) history
	Just openspace -> guardPathLoop m (newpos pos direction) direction (S.insert (pos,direction) history)

part2 m =length $ filter (==True) $  do
	newWallLocation <- filter (/= findstart m) (guardPathList m)
	return (guardPathLoop (M.adjust (const '#') newWallLocation m) (findstart m) (finddir m) S.empty)

