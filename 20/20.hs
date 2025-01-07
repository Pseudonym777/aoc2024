import Data.List
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe


part1 = do
    input <- readFile "input.txt";print ""
    let (maze,start,end) = parseInput input
    let [startDs, endDs] = map (distances maze) [start,end]
    let mazelength = startDs M.! end
    print   $ length $ filter (<(-99)) $ map (subtract mazelength) $ cheatvalues startDs endDs
part2 = do
    input <- readFile "input.txt";print ""
    let (maze,start,end) = parseInput input
    let [startDs, endDs] = map (distances maze) [start,end]
    let mazelength = startDs M.! end
    print mazelength
    print   $length  $  filter (<(-99)) $ map (subtract mazelength) $ longcheatvalues startDs endDs

cheatvalues startDs endDs =  concatMap (\(p,d) -> map (2+d +) $ mapMaybe (flip M.lookup endDs) $ cheatpairs 2 p) $ M.toList startDs

longcheatvalues startDs endDs =  concat [concatMap (\(p,d) -> map (n+d +) $ mapMaybe (flip M.lookup endDs) $ cheatpairs n p) $ M.toList startDs | n <- [2..20]]

parseInput input = (openspaces,start,end) where
    withcoords = concat $ zipWith zip infinitegrid (lines input)
    openspaces = S.fromList .map fst . filter (flip elem "SE.". snd)  $ withcoords
    start = fst . head $ filter ((== 'S'). snd) $ withcoords
    end = fst . head $ filter  ((== 'E') . snd)  $ withcoords


distances grid x = mconcat . map (\(n,s) ->M.fromList . flip zip (repeat n) $ S.toList s) . zip [0..] $ go S.empty (S.singleton x) where
    go previous frontier
        |S.null frontier = []
        |otherwise = frontier:go frontier (foldMap (S.intersection grid . S.fromList . adjacent) frontier S.\\ (S.union frontier previous))


adjacent (x,y) = [(a,b) | a <- [x-1..x+1],b <- [y-1..y+1], abs (x-a) + abs (y-b) == 1]

cheatpairs n (x,y) = [(a,b) | a <- [x-n..x+n],b <- [y-n..y+n], abs (x-a) + abs (y-b) == n] --horribly inneficient O(n^2)

infinitegrid::[[(Int,Int)]]
infinitegrid = [[(x,y)|x <- [0..]]|y<-[0..]]