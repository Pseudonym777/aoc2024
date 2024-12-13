import qualified Data.Map as M
import qualified Data.Set as S
import Text.Read
import Data.Bifunctor
import Control.Monad
import Data.Maybe
import Data.List
import Data.Function
main::IO ()
main = do
    input <- readFile "input.txt"
    print $sum $  map cost2 $ regions $ parseInput input


grid = [[(x,y)|x <- [0..]]|y<- [0..]]

parseInput input = M.fromList . concat $ zipWith zip grid (words input)

cost1 r = area r * perimeter r
cost2 r = area r * sidecount r
area r = S.size r

perimeter r = sum $ map S.size (boundaries r)
boundaries r = [S.difference r (S.map f r)|f <- [first (+1),first (subtract 1),second (+1),second (subtract 1)]]

sidecount r = sum $ map countEdges (rotations r) where
    rotatePoint (x,y) = (-y,x)
    rotations xs = take 4 $ iterate (S.map rotatePoint) xs
    countEdges shape = S.size . S.filter (isLeftEdge shape) $ shape
    isLeftEdge shape (x,y) = and [not $ S.member (x-1,y) shape,or [not $ S.member (x,y-1) shape,S.member (x-1,y-1) shape]]



regions m = case M.minViewWithKey m of
    Just (kv,rest) -> let (r,remaining) = region kv rest in r:regions remaining
    Nothing -> []

region ((x,y),garden) gardenMap =  foldr ($) (S.singleton (x,y),gardenMap) $ map propagate directions
    where
        directions = [(x-1,y),(x+1,y),(x,y+1),(x,y-1)]
        propagate newp (regionset, m) = fromMaybe (regionset,m) $ do
            newg <- M.lookup (newp) m
            guard (newg == garden)
            return $ (first (S.union regionset)) $ region (newp,newg) (M.delete (newp) m)