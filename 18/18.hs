import Data.Maybe
import qualified Data.Set as S
import Data.List
import qualified Data.Sequence as Seq
main = part2
part1 = do
    input <- readFile "input.txt"
    print $ fromJust $ pathLength $ parseInput 1024 input

part2 = do --brute force. takes a bit but is fast enough to work ()
    input <- readFile "input.txt"
    let blocked =  fromJust $ find (\n -> isNothing $  pathLength $ parseInput n input) [1024..]
    print $ lines input !! (blocked-1)

part2fancy = do
    input <- readFile "input.txt"
    print $ touchedWalls S.empty S.empty S.empty (map readLine $ lines input)
    
part2bs = do
    input <- readFile "input.txt"
    let blocked n = isNothing . pathLength $ parseInput n input
    print $ (lines input !!) . (subtract 1) $  binarysearch blocked (Seq.fromList [2..length $ lines input])

binarysearch p as = case s as of
    (Seq.Empty,Seq.Empty) -> error "empty seq"
    (xs Seq.:|> x,Seq.Empty) -> x
    (xs Seq.:|> x,y Seq.:<| ys ) -> if p x then binarysearch p (xs Seq.:|> x) else if p y then y else binarysearch p ys



s Seq.Empty = (Seq.empty,Seq.empty)
s (x Seq.:<| Seq.Empty) = (Seq.singleton x,Seq.empty)
s ((x Seq.:<| xs) Seq.:|> y) = (\(a,b) (as,bs) -> (a Seq.:<|as, bs Seq.:|> b)) (x,y) $ s xs

data Wall = L | R deriving(Show,Eq,Ord,Enum)


touchLeft (x,y) lefts 
    |x == 0 = True
    |y == 70 = True
    |any (flip S.member lefts) (adjacent (x,y)) = True
    |otherwise = False
touchRight (x,y) rights 
    |x == 70 = True
    |y == 0 = True
    |any (flip S.member rights) (adjacent (x,y)) = True
    |otherwise = False


touchedWalls lefts rights empties [] = error $ show (S.intersection lefts rights)
touchedWalls lefts rights empties (x:xs)
    |S.member x lefts' && S.member x rights' = x
    |otherwise = touchedWalls lefts' rights' empties' xs
        where
        newtouched = floodfill empties x
        lefts' = if touchLeft x lefts then lefts <> newtouched else lefts
        rights' = if touchRight x rights then rights <> newtouched else rights
        empties' = if touchRight x rights || touchLeft x lefts then empties S.\\ newtouched else S.insert x empties

touchpropagate empties fulls x = (newempties,newfulls) where
    anytouching =  any (flip S.member fulls) $  adjacent x 
    adjempties = S.intersection empties (S.fromList $ adjacent x)
    newempties = empties S.\\ newempties
    newfulls = fulls <> adjempties

adjacent (x,y) = [(a,b) | a <- [x-1..x+1],b <- [y-1..y+1]]






parseInput n input = grid where
    corrupted = S.fromList . map readLine . take n $ lines input
    grid = S.fromList [(x,y) | y <- [0..70],x <- [0..70]] S.\\ corrupted
readLine line = read $ '(':line++")"

step::S.Set (Int,Int) -> (Int,Int) -> S.Set (Int,Int)
step grid (x,y) = S.intersection grid $ S.fromList [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

pathLength grid = go (S.empty) (S.singleton (0,0)) where
    go previous frontier
        |S.member (70,70) frontier = Just 0
        |S.null frontier = Nothing
        |otherwise = fmap (+1) $ go frontier (foldMap (step grid) frontier S.\\ (S.union frontier previous))

floodfill grid x = go S.empty (S.singleton x) where
    go previous frontier
        |S.null frontier = S.empty
        |otherwise = frontier <> go frontier (foldMap (S.intersection grid . S.fromList . adjacent) frontier S.\\ (S.union frontier previous))