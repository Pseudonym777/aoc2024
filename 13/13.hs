import Control.Monad
import Data.Maybe
import Data.Char
import Data.List
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

handleZeroDet::[Int] -> [Int] -> [Int] -> Maybe (Int,Int)
handleZeroDet [x1,y1] [x2,y2] [x,y]
    |x1*3 > y1 = handleZeroDet  [x2,y2] [x1,y1] [x,y]
    |not $ gcd x1 x2 `divides` x = Nothing
    |xpref = dioph x1 x2 x
    |otherwise = fmap (\(u,v) -> (v,u)) $ dioph x2 x1 x
        where 
            xpref = x1*3 > y1
            dioph a b c = fmap (\n -> (div ( c - ( b * n)) a,n)) $ findIndex ( mod c a ==) $ iterate (\n -> mod (n + b) a) 0
             --euclid algorithm or something would be much faster



divides x y = mod y x == 0

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