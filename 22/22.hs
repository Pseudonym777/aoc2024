import Data.Bits
import qualified Data.Map as M
main = part2 
part1 = do
    input <- readFile "input.txt"
    print $ sum $ map (\x -> iterate updatesecret x !! 2000) $ parseInput input
part2 = do
    input <- readFile "input.txt"
    print $maximum $ M.elems $ M.unionsWith (+) $ map bananamap (parseInput input)

updatesecret = m (*2048) . m (flip div 32) . m (*64)

m::(Int -> Int) -> Int -> Int   
m f n = mod (xor (f n) n) 16777216

parseInput input = map read $ lines input


diffseq = zipWith subtract <*> tail

quintuples (a:b:c:d:e:xs) = [a,b,c,d,e]:quintuples (b:c:d:e:xs)
quintuples _ = []

banana xs = (diffs,value)where
    costs = map (flip mod 10) xs
    diffs = diffseq costs
    value = last costs


bananamap x = M.fromListWith (flip const) $ map banana . quintuples . take 2000 $ (iterate updatesecret x)
