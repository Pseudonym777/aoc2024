
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe

part1::IO ()
part1 = do
    input <- readFile "input.txt"
    print $ product $ M.elems $ M.fromListWith (+) $ (map (,1)) $ mapMaybe (quadrant . uncurry (newbotPos 100)) $ parseInput input

-- any movement along the grid will be periodic with period 101 in x and 103 in y, so if the picture is clustered in both x and y this finds it
part2:: IO () 
part2 = do
    input <- readFile "input.txt"
    let botStart = parseInput input
    let xaligned = snd . minimum  $ zip (map (alignmentx . map fst) $ iterate (map moveBot) botStart) [0..100]
    let yaligned = snd . minimum  $ zip (map (alignmenty . map fst) $ iterate (map moveBot) botStart) [0..100]
    print $ take 1 [x |x <- [0..], mod x 101 == xaligned,  mod x 103 == yaligned ] 
    -- again could be smarter and use crt but 10000 is pretty small for just some modular arithmetic

--just used for testing to notice clustering
printboard botset  = show [[if S.member (x,y) botset then 'B' else ' '|x <- [0..101]]|y <- [0..boundy]]

--counts unique x/y values, not very general for different input but I noticed this would work when looking at the first few grids
--more general something like computing the variance in x or something
alignmentx = S.size . S.fromList . map fst
alignmenty = S.size . S.fromList . map snd 

parseBot line = (position,velocity)
    where [position,velocity] = map (read . (\s -> '(':s ++ ")") . drop 2) $ words line

parseInput = map parseBot . lines

newbotPos time (x,y) (vx,vy) = (mod (x + vx * time) 101 ,mod (y + vy * time) 103)

moveBot::((Int,Int),(Int,Int)) -> ((Int,Int),(Int,Int))
moveBot ((x,y),vs) = (newbotPos 1 (x,y) vs,vs)

quadrant (x,y)
    |x*2+1 > 101 && y*2+1 > 103 = Just 0
    |x*2+1 > 101 && y*2+1 < 103 = Just 1
    |x*2+1 < 101 && y*2+1 > 103 = Just 2
    |x*2+1 < 101 && y*2+1 < 103 = Just 3
    |otherwise = Nothing



