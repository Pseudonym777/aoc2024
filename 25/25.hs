part1 = do
    input <- readFile "input.txt"
    let locks = map concat $ filter (/= [""]) $ groupBy (\x y -> x /= "" && y /= "") $ lines input
    print $ flip div 2 $ length [() | a <- locks, b <- locks, fits a b]

fits [] [] = True
fits ('#':as) ('#':bs) = False
fits (a:as) (b:bs) = fits as bs