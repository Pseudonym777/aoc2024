import Data.Tuple
import qualified Data.Map as M
import qualified Data.Set as S 
import Control.Monad
import Data.Bifunctor
import Data.List
import Data.Ord
parseInput = map parseline . lines
parseline (a:b:dash:c:[d]) = (a:[b],c:[d])

frompairs::[(String,String)] -> M.Map String (S.Set String)
frompairs xs = M.unionWith (<>) (fp xs) (fp $ map swap xs) where
    fp xs = M.fromListWith (<>) $ map (fmap S.singleton) xs

triples pairs = S.unions $  do
    a <- M.keys pairs
    b <- S.toList $ pairs M.! a
    return $ S.map (\x -> (S.fromList [a,b,x])) $ S.intersection (pairs M.! a) (pairs M.! b)

part1 = do
    input <- readFile "input.txt"
    print  $  S.size . S.filter hastname . triples . frompairs$ parseInput input

part2 = do
    input <- readFile "input.txt"
    print $map spacetocomma . unwords . S.toList  . maximumBy (comparing S.size) . S.toList . buildSimplexes  . frompairs . parseInput $ input

spacetocomma ' ' = ','
spacetocomma x = x

removeSimplex m = do
    ((seed,seededges),rest) <- M.minViewWithKey $ M.mapWithKey S.insert m
    let simplex = foldl1 S.intersection (S.map (rest M.!) seededges)
    return simplex
hastname s = not . S.null $ S.filter (\x -> head x == 't') s 


buildSimplexes edges = bs (M.toList edges) (S.singleton S.empty) where
    bs [] s = s
    bs (x:xs) s = bs xs $ case first S.minView $ S.partition (flip S.isSubsetOf (snd x)) s of
        (Nothing,rest) -> S.insert (S.singleton (fst x)) rest
        (Just (simplex,_),rest) -> S.insert (S.insert (fst x) simplex) rest


