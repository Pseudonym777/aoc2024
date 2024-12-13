import Data.Function
import Data.Char
import Text.Read
import Data.Maybe
import Data.List
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Sequence as S
main::IO ()
main = do
    input <- readFile "input.txt"
    let rules = mapMaybe parseRule (lines input)
    let orders = filter (not . null) $ mapMaybe parseOrder (lines input)
    print $ sum $ map (middle . fixOrder rules) $ filter (not .followsAll rules) orders

part1 rules orders= length . filter (followsAll rules) $ orders
part2 rules orders= sum $ map (middle . fixOrder rules) $ filter (not .followsAll rules) orders
parseRule::String -> Maybe (Int,Int)
parseRule xs = readMaybe $ '(':map bartocomma xs ++ ")"
   where bartocomma '|' = ','
         bartocomma  x = x

parseOrder::String -> Maybe [Int]
parseOrder xs = readMaybe $ '[':xs ++ "]"

follows::(Int,Int) -> [Int] -> Bool
follows (x,y) order =(/= Just False) $  do
    first <- findIndex (==x) order
    second <- findIndex (==y) order
    return (first < second)

followsAll rules order = all (flip follows order ) rules

fixOrder::[(Int,Int)] -> [Int] -> [Int]
fixOrder rules order = go $ trim $ M.union (M.fromListWith (++) (map (fmap singleton) $filter (flip elem order . fst ) rules)) (M.fromList $ map (,[]) order)
    where
    trim::M.Map Int [Int] -> M.Map Int [Int]
    trim m = M.map (filter (flip M.member m)) m
    go::M.Map Int [Int] -> [Int]
    go m
      |M.null m = []
      |otherwise = let (bigs,rest) = M.partition null m in M.keys bigs ++ go (trim rest)
    


middle xs = go (S.fromList xs)
   where 
       go ((x S.:<| xs) S.:|> y) = go xs
       go (x S.:<| S.Empty) = x
       go S.Empty = error ("even length list, no middle:" ++ show xs)
