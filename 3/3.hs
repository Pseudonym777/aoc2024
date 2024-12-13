
import Data.Function
import Data.Char
import Text.Read
import Data.Maybe
import Data.List
main::IO ()
main = do
    input <- readFile "input.txt"
    print $ part2 input

part1 = sum . muls
part2 = sum . muls . removeDisabled False
muls::String -> [Int]
muls = catMaybes . findmulMaybes . groupBy (on (==) isDigit)
    where findmulMaybes ("mul(":x:",":y:(')':xs):ys) = liftA2 (*) (readMaybe x) (readMaybe y):findmulMaybes (xs:ys)
    findmulMaybes ((x:xs):ys) = findmulMaybes (xs:ys)
    findmulMaybes ([]:xs) = findmulMaybes xs
    findmulMaybes [] = []

removeDisabled _ [] = []
removeDisabled _ ('d':'o':'(':')':xs) = removeDisabled False xs
removeDisabled _ ('d':'o':'n':'\'':'t':'(':')':xs) = removeDisabled True xs
removeDisabled True (x:xs) = removeDisabled True xs
removeDisabled False (x:xs) = x:removeDisabled False xs


