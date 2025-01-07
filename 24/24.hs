import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Data.List
import Data.Maybe
import Data.Bits
import Data.Ord
data Wire = ON | OFF | Gate (Bool -> Bool -> Bool) String String

main = part2

part1 = do
    input <- readFile "input.txt"
    print $ fromBinary $ getchars 'z' $ evalWires $ parseInput input
part2 = do
    input <- readFile "input.txt"
    let m1 = parseInput2 input
    let (_,a1,b1,m2) = findswap $ m1
    let (_,a2,b2,m3) = findswap $ m2
    let (_,a3,b3,m4) = findswap $ m3
    let (finalLoss,a4,b4,m5) = findswap $ m4
    print $ sort [a1,a2,a3,a4,b1,b2,b3,b4]

parseInput input = wiremap where
    wiremap = M.fromList . map parseLine . filter (not . null) .lines $ input
    parseLine (a:b:c:':':' ':['1']) = (a:b:[c],ON)
    parseLine (a:b:c:':':' ':['0']) = (a:b:[c],OFF)
    parseLine (a:b:c:' ':'A':'N':'D':' ':d:e:f:' ':'-':'>':' ':g:h:[i]) = (g:h:[i],Gate (&&) (a:b:[c]) (d:e:[f]))
    parseLine (a:b:c:' ':'O':'R':' ':d:e:f:' ':'-':'>':' ':g:h:[i]) = (g:h:[i],Gate (||) (a:b:[c]) (d:e:[f]))
    parseLine (a:b:c:' ':'X':'O':'R':' ':d:e:f:' ':'-':'>':' ':g:h:[i]) = (g:h:[i],Gate (/=) (a:b:[c]) (d:e:[f]))

parseInput2 input = wiremap where
    wiremap = M.fromList . map parseLine . tail . dropWhile (not . null) .lines $ input
    parseLine (a:b:c:' ':'A':'N':'D':' ':d:e:f:' ':'-':'>':' ':g:h:[i]) = (g:h:[i],Gate (&&) (a:b:[c]) (d:e:[f]))
    parseLine (a:b:c:' ':'O':'R':' ':d:e:f:' ':'-':'>':' ':g:h:[i]) = (g:h:[i],Gate (||) (a:b:[c]) (d:e:[f]))
    parseLine (a:b:c:' ':'X':'O':'R':' ':d:e:f:' ':'-':'>':' ':g:h:[i]) = (g:h:[i],Gate (/=) (a:b:[c]) (d:e:[f]))

execute m n k =fromBinary . getchars 'z' $ evalWires (M.unions [m,xs,ys]) where
    xs = M.fromList $ zip (map (('x':). digitstring) [0..44]) (toBinary n ++ repeat OFF) 
    ys = M.fromList $ zip (map (('y':). digitstring) [0..44]) (toBinary k ++ repeat OFF) 

digitstring n
    |n < 10 = '0':show n
    |otherwise = show n

f a b= do input <- readFile "input.txt";return $ execute (parseInput2 input) a b

gooddeps k v = lowerdeps where
    lowerdeps = head k /= 'z' || and [S.member ('x':show n) v && S.member ('y':show n) v| n <-[0..place]]
    place = read $ tail k

findswap m = let deps = dependents m in minimumBy (comparing (\(a,_,_,_) -> a)) $ do
    swap1 <- M.keys m
    swap2 <- M.keys m
    guard $ S.notMember swap2 $ deps M.! swap1
    guard $ S.notMember swap1 $ deps M.! swap2
    let newmap = swap m swap1 swap2
    let loss = sum $ map (\(x,y) -> popCount $ xor (x + y)  (execute newmap x y))  testpairs
    return (loss,swap1,swap2,newmap)

evalWires m = M.map evalWire m where
    evalWire ON = True
    evalWire OFF = False
    evalWire (Gate f a b) = f (evalWire (m M.! a)) (evalWire (m M.! b))

dependents m = d where
    d = M.mapWithKey dependent m
    dependent name ON = S.singleton name
    dependent name OFF = S.singleton name
    dependent name (Gate f a b) = S.insert name $ S.union (fromMaybe S.empty $ M.lookup b d) (fromMaybe S.empty $M.lookup a d)

getchars c =M.elems .  M.filterWithKey (\k v -> ([c]==) . take 1 $ k) 

fromBinary xs = fb (reverse xs) 0 where
    fb [] acc = acc
    fb (x:xs) acc = fb xs $ fromEnum x + 2*acc
toBinary n
    |n == 0 = [OFF]
    |n == 1 = [ON]
    |mod n 2 == 1 = ON:toBinary (div n 2)
    |mod n 2 == 0 = OFF:toBinary (div n 2)


swap m a b = M.insert a bval . M.insert b aval $ m where
    aval = m M.! a
    bval = m M.! b

testpairs::[(Int,Int)]
testpairs = [(mod (101^n) (2^45),mod (29^n) (2^45)) | n <- [0..1]]

