import qualified Data.Map as M
import Text.Read
main::IO ()
main = do
    input <- readFile "input.txt"
    print $  part2 $ map read $ words $ input


part1 ns = length $ iterate (>>= stonerule) ns !! 25 --naive solution
part2 ns = sum $ M.elems $ iterate (countMapApply stonerule) (M.fromList $ map (,1) ns) !! 75 

stonerule n
    |n == 0 = [1]
    |evendigits n = splitdigits n
    |otherwise = [2024*n]

evendigits n = mod n 100 < 10

splitdigits::Int -> [Int]
splitdigits n = [read l, read r]
    where 
        s = show n
        l = length s
        (l,r) = 

countMapApply:: (Ord a,Ord b) =>(a ->[b]) -> M.Map a Int -> M.Map b Int
countMapApply f m = M.fromListWith (+) $ concatMap (\(v,c) -> [(nv,c) | nv <- f v]) (M.toList m)
