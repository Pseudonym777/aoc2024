import qualified Data.Map as M
import qualified Data.Set as S
import Data.Bifunctor
type Point = (Int,Int)
part1 = do
    input <- readFile "input.txt"
    let (wallset,boxset,directions,robotpos) = parseInput input
    print $score $ snd $  foldl (uncurry (moveBot wallset)) (robotpos,boxset) directions

part2 = do
    input <- readFile "input.txt"
    let (wallset,boxset,directions,robotpos) = parseInput input
    print $score $ snd $  foldl (uncurry (moveBot2 (widenwalls wallset))) ((widenbot robotpos),(widenboxes boxset)) directions

widenboxes = S.map (first (*2))

widenbot = first (*2)

score = sum . map (\(x,y) -> x + 100* y) .  S.toList

moveBot::S.Set Point -> Point -> S.Set Point -> (Point -> Point) -> (Point, S.Set Point)
moveBot wallset robotpos boxset direction
    |S.member r wallset = (robotpos,boxset)
    |S.member r boxset = case moveBox wallset direction boxset r of
        Nothing -> (robotpos,boxset)
        Just newboxpos -> (r,S.delete r . S.insert newboxpos $ boxset)
    |otherwise = (r,boxset)
        where r = direction robotpos




moveBox wallset direction boxset boxpos
    |S.member b wallset = Nothing
    |S.member b boxset = moveBox wallset direction boxset b
    |otherwise = Just b
        where b = direction boxpos

moveBot2::S.Set Point -> Point -> S.Set Point -> (Point -> Point) -> (Point, S.Set Point)
moveBot2 wallset robotpos boxset direction
    |S.member r1 wallset  = (robotpos,boxset)
    |otherwise = case pushedBoxes of
        [] -> (r1,boxset)
        [x] -> case moveBox2 wallset direction boxset x of
            Nothing -> (robotpos,boxset)
            Just newboxset -> (r1,newboxset)
        (x:xs) -> error "impossible for robot to directly push 2 boxes"
        where 
            r1 = direction robotpos
            r2 = first (subtract 1) r1
            pushedBoxes = S.toList $ S.intersection (S.fromList [r1,r2]) boxset


moveBox2::S.Set Point -> (Point -> Point) -> S.Set Point -> Point -> Maybe (S.Set Point)
moveBox2 wallset direction boxset boxpos
    |S.member b1 wallset = Nothing
    |S.member b2 wallset = Nothing
    |otherwise = case pushedBoxes of 
        [] -> Just (S.delete boxpos . S.insert b1 $ boxset)
        (x:xs) -> do 
            newboxset <- moveBox2 wallset direction boxset x
            moveBox2 wallset direction newboxset boxpos
        where
            b1 = direction boxpos
            b2 = first (+1) b1
            b3 = first (subtract 1) b1
            pushedBoxes = S.toList $ S.delete boxpos $ S.intersection (S.fromList [b1,b2,b3]) boxset


parseInput input = (wallset,boxset,directions,robotpos) where
    (g,directions) = second (map carrotToDirection. concat) . splitOnFirst (== "") $ lines input
    gridMap  = M.fromList . concat $ zipWith zip infinitegrid g 
    wallset = M.keysSet . M.filter (=='#') $ gridMap
    boxset =  M.keysSet . M.filter (=='O') $ gridMap
    robotpos = fst . M.findMin . M.filter (=='@') $ gridMap

carrotToDirection::Char -> Point -> Point
carrotToDirection '^' = second (subtract 1)
carrotToDirection 'v' = second (+1)
carrotToDirection '>' = first (+1)
carrotToDirection '<' = first (subtract 1)


infinitegrid = [[(x,y)|x <- [0..]] |y <- [0..]]

splitOnFirst p xs = (takeWhile (not . p) xs,drop 1 $ dropWhile (not . p) xs )


--used for debugging 
test = do
    input <- readFile "input.txt"
    let (wallset,boxset,directions,robotpos) = parseInput input 
    putStr $ showgrid 20 (widenboxes boxset) (widenwalls wallset) (widenbot robotpos)
    let (p,bs) =  foldl (uncurry (moveBot2 (widenwalls wallset))) ((widenbot robotpos),(widenboxes boxset)) directions
    putStr $ showgrid 20 bs (widenwalls wallset) p
    print $ p

widenwalls wallset = S.union (S.map l wallset) (S.map r wallset) where
    l = first (*2)
    r = first ((+1) . (*2))

showcell boxset wallset robotpos (x,y)
    |S.member (x,y) boxset = '['
    |S.member (x-1,y) boxset = ']'
    |S.member (x,y) wallset = '#'
    |(x,y) == robotpos = '@'
    |otherwise = '.'

showgrid n bs ws r = unwords $map ((++"\n") . map (showcell bs ws r)) [[(x,y) | x <- [0..n]] | y <- [0..n]]