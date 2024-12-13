import Control.Monad
import Data.Maybe
import Data.Char
main::IO ()
main = do
    input <- readFile "input.txt"
    print $ part2 input

parseInput input =go (lines input)
    where
        go [] = []
        go ("":xs) = go xs
        go (x:y:z:xs) = (extractnums x,extractnums y, extractnums z):go xs

extractnums::String -> [Int]
extractnums xs = map read . filter (not . null) $ map (filter isDigit) $ words xs


part1 =sum . map score .  mapMaybe (\(x,y,z) -> cramer x y z) . parseInput
part2 = sum . map score .  mapMaybe (\(x,y,z) -> cramer x y (map (+10000000000000) z)) . parseInput

score (x,y) = 3*x + y

det [x1,y1] [x2,y2] = x1*y2 - x2*y1

handleZeroDet [x1,y1] [x2,y2] [x,y]
    |x1*3 > y1 = do n <- exactdiv x x1; return (n,0)
    |otherwise = do n <- exactdiv y y1; return (0,n)

cramer::[Int] -> [Int] -> [Int] -> Maybe (Int,Int)
cramer m1 m2 b = case det m1 m2 of
    0 -> handleZeroDet m1 m2 b
    d -> do
        x <- det b m2 `exactdiv` d
        y <- det m1 b `exactdiv` d
        guard (x >= 0)
        guard (y >= 0)
        return $ (x,y)

exactdiv a 0 = Nothing
exactdiv a b = case divMod a b of
    (n,0) -> Just n
    otherwise -> Nothing