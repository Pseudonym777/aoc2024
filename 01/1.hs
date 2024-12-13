
import Data.List (transpose, sort)
import qualified Data.Map as M
main = do
	input <- readFile "input.txt"
	let [l1,l2]::[[Int]] = transpose .  map (map read .words) . lines  $ input
	print $ part2 l1 l2

distance x y = abs $ x - y

counts = M.fromListWith (+) . map (,1)

distancesum xs ys = sum $ zipWith distance xs ys

part1 l1 l2 = distancesum (sort l1 ) (sort l2)


part2 l1 l2 = M.foldr (+) 0 $ M.intersectionWithKey (\n c1 c2 -> n*c1*c2) (counts l1) (counts l2)