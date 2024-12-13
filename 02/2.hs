main = do
    input <- readFile "input.txt"
    let levels = map (map read . words) . lines $ input
    print $ part2 levels


part1 = length . filter monotonic
part2 = length . filter mostlyMonotonic

increasing [] = True
increasing [x] = True
increasing (x:y:xs) = safegap x y && increasing (y:xs)

safegap x y = y - x < 4 && x < y

decreasing = increasing . map negate

monotonic x = increasing x || decreasing x


mostly p xs = any p $ dampeners xs

dampeners [] = []
dampeners (x:xs) = xs:(map (x:)) (dampeners xs)  

mostlyMonotonic x = mostly increasing x || mostly decreasing x 