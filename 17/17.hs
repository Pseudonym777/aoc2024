import Data.Bits
import Control.Monad
import Data.List
data Computer = Comp {regA::Integer,regB::Integer,regC::Integer,pointer::Integer,opList::[Integer], output::[Integer]} deriving(Show)

part1 = do
    input <- readFile "input.txt"
    print $ runComputer $ parseInput input

part2 = do
    input <- readFile "input.txt"
    print $ solveAllBitTriplets (parseInput input) [0]

-- by inspecting the input, it only uses the last 3 bits of A, then truncates each loop, so I can just construct the output 3 bits at a time
solveBitTriplet c oldA = do
    newbits <- [0..7] 
    let newA = 8*(oldA) + newbits
    guard $ prefix (runComputer c{regA = newA}) (reverse (opList c))
    return newA

prefix [] ys = True
prefix (x:xs) (y:ys) = x == y && prefix xs ys
prefix xs [] = False

solveAllBitTriplets c n =minimum . last . takeWhile (not . null) $  iterate (>>= solveBitTriplet c) n

parseInput input = Comp a b c 0 program [] where
    numbers = map (filter (flip elem "1234567890,")) $ lines input
    [l1,l2,l3,l4,l5] = numbers
    a = read l1
    b = read l2
    c = read l3
    program = read $ '[':l5 ++ "]"

combo c operand = case operand of
    4 -> regA c
    5 -> regB c
    6 -> regC c
    7 -> error "combo operator 7, illegal"
    otherwise -> operand

mvPointer c = c{pointer = pointer c + 2}

adv c operand = mvPointer c{regA = div (regA c) (2^ combo c operand)}

bxl c operand = mvPointer c{regB = xor (regB c) operand}

bst c operand = mvPointer c{regB = mod (combo c operand) 8} 

jnz c operand = case (regA c) of
    0 -> mvPointer c
    otherwise -> c{pointer = operand}

bxc c operand = mvPointer c{regB = xor (regB c) (regC c)}

out c operand = mvPointer c{output = mod (combo c operand) 8: output c }

bdv c operand = mvPointer c{regB = div (regA c) (2^ combo c operand)}

cdv c operand = mvPointer c{regC = div (regA c) (2^ combo c operand)}

instructionFromCode n = case n of
    0 -> adv
    1 -> bxl
    2 -> bst
    3 -> jnz
    4 -> bxc
    5 -> out
    6 -> bdv
    7 -> cdv
    otherwise -> error "invalid opcode"

runComputer c = case nextstep of
        Just valid -> runComputer valid
        Nothing -> output c
    where 
        nextstep = do
            instructionCode <- safeIndex (opList c) (pointer c)
            let instruction = instructionFromCode instructionCode
            operand <- safeIndex (opList c) (pointer c + 1) 
            return $ instruction c operand

safeIndex xs n = case drop (fromInteger n) xs of
    (x:ys) -> Just x
    [] -> Nothing











