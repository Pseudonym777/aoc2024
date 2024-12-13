import qualified Data.Sequence as S
import Data.Char
import Data.Foldable (toList)
import Data.Maybe
import Data.Bifunctor
main::IO ()
main = do
    input <- readFile "input.txt"
    print  $part2 input
parseInput :: String -> S.Seq (Maybe Int)
parseInput xs = go True 0 xs
    where 
        go _ _ [] = S.empty
        go False n (x:xs) = S.replicate (digitToInt x) Nothing S.>< go True n xs
        go True n (x:xs) = S.replicate (digitToInt x) (Just n)  S.>< go False (succ n) xs


parseInput2::String -> S.Seq (Int,Maybe Int)
parseInput2 xs = go True 0 xs
    where
        go _ _ [] = S.empty
        go False n (x:xs) = (digitToInt x,Nothing) S.:<| go True n xs
        go True n (x:xs) = (digitToInt x,Just n) S.:<| go False (succ n) xs


movefiles (xs S.:|> Nothing) = movefiles xs S.:|> Nothing
movefiles (Nothing S.:<| (xs S.:|>x)) = x S.<| (movefiles xs S.|> Nothing)
movefiles (x S.:<| xs) = x S.<| movefiles xs
movefiles _ = S.empty



partialchecksum startindex filesize fileid =(fileid *) .  product $ take filesize [startindex..]

checksum = sum . zipWith (*) [0..]  . catMaybes . toList

part1 xs = checksum $ movefiles . parseInput $ xs


insertSlow::S.Seq (Int,Maybe Int) -> S.Seq (Int,Maybe Int)
insertSlow (xs S.:|> (_,Nothing)) = insertSlow xs
insertSlow ((l,Just n) S.:<| xs) = (l,Just n) S.:<| insertSlow xs
insertSlow ((0,Nothing) S.:<| xs) = insertSlow xs
insertSlow (b S.:<| xs) = let (filled,rest) = insertOne (b S.:<| xs) in filled S.>< insertSlow rest
insertSlow S.Empty = S.empty


insertOne::S.Seq (Int,Maybe Int) -> (S.Seq(Int,Maybe Int),S.Seq (Int,Maybe Int))
insertOne (b S.:<| S.Empty) = (S.singleton b,S.Empty)
insertOne (xs S.:|> (n,Nothing)) = second (S.:|> (n,Nothing)) (insertOne xs)
insertOne (((blocksize,Nothing) S.:<| xs) S.:|> (filesize,Just idnum))
    |filesize > blocksize = second (S.:|> (filesize,Just idnum)) $insertOne ((blocksize,Nothing) S.:<| xs)
    |filesize == blocksize = (S.singleton (filesize,Just idnum), xs S.:|> (filesize,Nothing) )
    |otherwise = bimap ((filesize,Just idnum)S.:<|) (S.:|> (filesize,Nothing)) $ insertOne ( (blocksize-filesize,Nothing) S.:<| xs)

blockseqtolist S.Empty = []
blockseqtolist ((size,idnum) S.:<|xs) = replicate size (fromMaybe 0 idnum) ++ blockseqtolist xs

part2 =sum .  zipWith (*) [0..] . blockseqtolist . insertSlow . parseInput2










