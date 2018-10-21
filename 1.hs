module AdventOfCode.One where

    import Data.Char (digitToInt)

    parse :: String -> [Int]
    parse = map digitToInt

    solve1 :: [Int] -> Int
    solve1 l@(x:xs) =  traverser (l ++ [x]) 0

    traverser (x:y:xs) acc | x == y = traverser (y:xs) (acc + y)
                           | otherwise = traverser (y:xs) acc
    traverser _        acc = acc

    solve2 :: [Int] -> Int
    solve2 xs = let
                    len = length xs
                in
                    rounder' xs (div len 2)

    rounder' :: [Int] -> Int -> Int
    rounder' xs step = rounder xs step (length xs) 0 0

    rounder :: [Int] -> Int -> Int -> Int -> Int -> Int
    rounder xs step len i s | i == len = s
                            | curr == xs!!ix = rounder xs step len (i + 1) (s + curr)
                            | otherwise = rounder xs step len (i + 1) s
                    where ix = mod (i + step) len
                          curr = xs!!i

    main :: IO ()
    main = do
        input <- readFile "input1"
        let result = solve1 (parse $ init input)
        -- let result = solve2 (parse $ init input)
        print result

