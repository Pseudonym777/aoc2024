import Control.Monad
import Data.Bifunctor
import Data.List
import qualified Data.Set as S
import qualified Data.Map as M

data ArrowKey = A | U | R | D | L deriving(Show,Enum,Eq,Ord)

data NumKey = NK Int | Finish deriving(Show,Eq,Ord)

part1 = do
    input <- readFile "input.txt"
    print $ sum $ map (\x -> complexity x * finalcost 2 x) $ lines input
part2 = do
    input <- readFile "input.txt"
    print $ sum $ map (\x -> complexity x * finalcost 25 x) $ lines input

complexity::String -> Int
complexity = read . init 

arrowCoord U = (1,0)
arrowCoord A = (2,0)
arrowCoord L = (0,1)
arrowCoord D = (1,1)
arrowCoord R = (2,1)

nkCoord (NK n)
    |n > 6 = (n-7,0)
    |n > 3 = (n-4,1)
    |n > 0 = (n-1,2)
    |otherwise = (1,3)
nkCoord Finish = (2,3)

parseNK 'A' = Finish
parseNK n = NK (read [n])

arrowMovement D = second (+ 1)
arrowMovement R = first (+ 1)
arrowMovement L = first (subtract 1)
arrowMovement U = second (subtract 1)
arrowMovement A = id


arrowbot bounds kin kout = if kin == kout then [[A]] else do
    newkey <- [(U)..]
    let newpos = arrowMovement newkey kin
    guard $ elem newpos $ bounds
    guard $ distance newpos kout < distance kin kout
    map (newkey:) $ arrowbot bounds newpos kout

recursivecosts::Int -> M.Map (ArrowKey,ArrowKey) Int 
recursivecosts 0 = M.fromList [((a,b),1) |a <- [(A)..],b <- [(A)..]]
recursivecosts n =  M.mapWithKey updatecosts prev where
    prev = recursivecosts (n-1)
    updatecosts (a,b) cost = minimum  $ map (sum . map (prev M.!) . tuples . (A:)) $ arrowbot (map arrowCoord [(A)..]) (arrowCoord a) (arrowCoord b)

finalcost bots output =sum $ map bestoptioncost options where
    bestoptioncost o = minimum $ map ((sum . map(costs M.!)) . tuples . (A:)) $ o
    options = map (uncurry (arrowbot bounds)) .  tuples . map nkCoord $   Finish: map parseNK output
    bounds = [(x,y) | x <-[0..3],y <- [0..3],(x,y) /= (0,3)]
    costs = recursivecosts bots
    

tuples (x:y:xs) = (x,y):(tuples (y:xs))
tuples _ = []

distance (x,y) (a,b) = abs (x - a) + abs (y - b) 
