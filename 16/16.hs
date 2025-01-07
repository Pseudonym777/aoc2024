import Data.Bifunctor
import Control.Monad
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (find)
import Data.Maybe
--not proud of today's code
main = part2
part1 = do
    input <- readFile "input.txt"
    let (nodes,start,end) = parseInput input
    print $ findpath nodes start end
part2 = do
    input <- readFile "input.txt"
    let (nodes,start,end) = parseInput input
    print $ findpaths nodes start end
data Direction = L | U | R | D deriving(Show,Eq,Ord,Enum)

forward L = first (subtract 1)
forward U = second (subtract 1)
forward R = first (+1)
forward D = second (+1)

turnright D = L
turnright d = succ d

turnleft L = D
turnleft d = pred d

type Point = (Int,Int)
findpath::S.Set Point -> Point -> Point -> Int
findpath nodes start end = exploreAll nodes (Heap 0 (start,R) []) where
    exploreAll::S.Set Point -> Heap Int (Point,Direction) -> Int
    exploreAll ns h@(Heap c (p,d) hs)
        |p == end = c
        |otherwise = exploreAll newnodes withMoves
            where
                withMoves = merge (fromList nextoptions) $ deleteMin h
                newnodes = S.delete p ns
                nextoptions = filter (flip S.member ns . fst . snd) [f,r,l]
                f = (c+1,(forward d p,d))
                r = (c+1001,(forward (turnright d) p,turnright d))
                l = (c+1001,(forward (turnleft d) p,turnleft d))

--findpaths::S.Set Point -> Point -> Point -> Int
findpaths nodes start end =backtrack end $M.map (fmap negate) .  M.mapKeys (second (turnleft . turnleft)) $ exploreAll (M.insert (start,R) (Nothing) $ constMapFromSet (withdirections nodes) Nothing) (Heap 0 (start,R) []) where
    exploreAll::M.Map (Point,Direction) (Maybe Int) -> Heap Int (Point,Direction) -> M.Map (Point,Direction) (Maybe Int)
    exploreAll bounds Empty  = error (show start)
    exploreAll bounds h@(Heap c (p,d) hs) = if p == end then (M.insert (p,d) (Just c) bounds) else
        case M.lookup (p,d) bounds of 
            Nothing -> exploreAll bounds (deleteMin h)
            Just bound -> if fromMaybe c bound < c then exploreAll bounds (deleteMin h) else
                exploreAll (M.insert (p,d) (Just c) bounds) (fromList (nextoptions (c,(p,d))) <> deleteMin h)

nextoptions (c,(p,d)) = [f,r,l] where
    f = (c+1,(forward d p,d))
    r = (c+1000,(p,turnright d))
    l = (c+1000,(p,turnleft d))
backtrack::Point -> M.Map (Point,Direction) (Maybe Int) -> Int
backtrack end bounds = S.size $ S.map (fst . snd) $ S.unions $ takeWhile (not . null) $ iterate (backtrackStepSet bounds) (S.fromList endwithalldirections) where
    endwithalldirections = do
        d <- [(L)..]
        let cost = M.lookup (end,d) bounds
        case cost of
            (Just (Just c)) -> return (c,(end,d))
            otherwise -> []

backtrackStep::M.Map (Point,Direction) (Maybe Int) -> (Int,((Int,Int),Direction)) -> [(Int,((Int,Int),Direction))]
backtrackStep bounds start = do
    (cost,location) <- nextoptions start
    guard (lookupboth location (Just cost) bounds)
    return (cost, location)

backtrackStepSet bounds=  S.fromList . (>>= backtrackStep bounds) . S.toList

lookupboth k v m = M.lookup k m == Just v

constMapFromSet::Ord a =>S.Set a -> b ->  M.Map a b
constMapFromSet s c = M.fromAscList $ zip (S.toAscList s) (repeat c)

withdirections::Ord a => S.Set a -> S.Set (a,Direction)
withdirections s = S.unions [S.map (,d) s | d <- [(L)..] ]

parseInput xs = (nodes,start,end) where
    withcoords = concat $ zipWith zip infinitegrid (lines xs) 
    nodes = S.fromList $ map fst $ filter (flip elem "ES." . snd) $ withcoords
    start = fst $fromJust $ find ((== 'S') . snd) withcoords
    end   = fst $ fromJust $ find ((== 'E') . snd) withcoords
        


infinitegrid = [[(x,y) | x <- [0..]] | y <- [0..]]

data Heap k a = Empty | Heap k a [Heap k a] deriving (Show)

instance Ord k => Semigroup  (Heap k a) where 
    (<>) = merge

instance Ord k => Monoid (Heap k a) where
    mempty = Empty

merge::Ord k => Heap k a -> Heap k a -> Heap k a
merge Empty h = h
merge h Empty = h
merge h1@(Heap k1 v1 hs1) h2@(Heap k2 v2 hs2)
    |k1 < k2 = Heap k1 v1 (h2:hs1)
    |otherwise = Heap k2 v2 (h1:hs2)

deleteMin Empty = Empty
deleteMin (Heap k v hs) = mconcat hs

viewMin Empty = Nothing
viewMin (Heap k v hs) = Just ((k,v), mconcat hs)

fromList:: Ord k => [(k,a)] -> Heap k a
fromList = mconcat . map (\(k,v) -> Heap k v [])

toList Empty = []
toList (Heap k v hs) = (k,v):toList (mconcat hs)

insert k v h = merge (Heap k v []) h 
