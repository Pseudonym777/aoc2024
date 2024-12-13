import Data.Maybe
import qualified Data.ByteString.Char8 as B
main::IO ()
main = do
    input <- B.readFile "input.txt"
    let problems = map parseLine  . B.lines $ input
    print $ part2 problems
    
part1 problems = sum . map fst . filter (uncurry $ isCalibrated [multInv,addInv]) $ problems
part2 problems = sum . map fst . filter (uncurry $ isCalibrated [catInv,multInv,addInv]) $ problems

parseLine::B.ByteString -> (Int,[Int])
parseLine xs = let (w:ws) = B.words xs in (readIntUnsafe . B.init $ w,map readIntUnsafe ws)

readIntUnsafe::B.ByteString -> Int
readIntUnsafe = fst . fromJust . B.readInt

isCalibrated::Eq a => [a -> a -> Maybe a] -> a -> [a] -> Bool
isCalibrated operators t xs = go (reverse xs) t where --easier to prune tree from reverse
    go [x] t = t == x
    go (x:xs) t = any (go xs) $ mapMaybe (($ x) . ($ t)) operators

addInv::Integral a => a -> a -> Maybe a
addInv x y
    | x > y = Just (x-y)
    |otherwise = Nothing

catInv::Integral a => a -> a -> Maybe a
catInv x y
  |y == 0 = Just x
  |mod x 10 == mod y 10 = catInv (div x 10) (div y 10)
  |otherwise = Nothing

multInv::Integral a => a -> a -> Maybe a
multInv x y = let (z,r) =quotRem x y in if r == 0 then Just z else Nothing