import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.Function
import Data.Tuple
main::IO ()
main = do
    input <- readFile "input.txt"
    print $ part2 input



parseGrid::String -> ((Int,Int),M.Map Char (S.Set (Int,Int)))
parseGrid input =((length . head $ lines input,length $ lines input),) $
    M.filterWithKey (\k v -> k/= '.') $ M.fromListWith S.union $ concat $ zipWith zip (lines input) (map  (map S.singleton) infinitegrid)

infinitegrid = [[(x,y)|x <- [0..]]|y<-[0..]]

pairs::Ord a => S.Set a -> [(a,a)]
pairs s = let xs = S.toList s in [(x,y) | x <- xs,y<-xs, x /= y]

nodes::[((Int,Int),(Int,Int))] -> [(Int,Int)]
nodes xs = concatMap (\((x,y),(a,b)) -> [(2*x-a,2*y-b),(2*a-x,2*b-y)]) xs

nodesLine::((Int,Int),(Int,Int)) -> ((Int,Int ) -> Bool)
nodesLine ((a,b),(x,y)) = (\(u,v) -> (y-b)*(u-x)==(v-y)*(x-a))

part1 input  =S.size $ S.filter inbounds $ M.foldr S.union S.empty $ M.map ( S.fromList .  nodes. pairs) $ g
    where 
        ((xbound,ybound),g) = parseGrid input
        inbounds (x,y) = and[x >= 0, x <xbound,y >=0, y < ybound]

part2 input = S.size $S.fromList $concat $map (\f -> filter f (concat grid)) $ concat $ M.elems $ M.map (map nodesLine . pairs) $ g
    where 
        ((xbound,ybound),g) = parseGrid input
        grid = [[(x,y)|x <- [0..xbound-1]]|y<-[0..ybound-1]]


