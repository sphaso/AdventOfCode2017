module AdventOfCode.Two where

    import Data.List

--   5 1 9 5
--   7 5 3
--   2 4 6 8
--   The first row's largest and smallest values are 9 and 1, and their difference is 8.
--   The second row's largest and smallest values are 7 and 3, and their difference is 4.
--   The third row's difference is 6.

    sethighlow :: [Int] -> Int
    sethighlow l@(x:xs) = highlow l x x

    highlow :: [Int] -> Int -> Int -> Int
    highlow [] h l = h - l
    highlow (x:xs) h l | x < l = highlow xs h x
                       | x > h = highlow xs x l
                       | otherwise = highlow xs h l

--   5 9 2 8
--   9 4 7 3
--   3 8 6 5

--   In the first row, the only two numbers that evenly divide are 8 and 2; the result of this division is 4.
--   In the second row, the two numbers are 9 and 3; the result is 3.
--   In the third row, the result is 2.

    setdivs xs = divs xs detros detros
            where sorted = sort xs
                  detros = reverse sorted

    -- xs = divisors, sorted ASC
    -- ys = dividends, sorted DESC
    divs :: [Int] -> [Int] -> [Int] -> Int
    divs (x:xs) []     o = divs xs o o
    divs (x:xs) (y:ys) o | x == y = divs (x:xs) ys o
                         | mod y x == 0 = div y x
                         | otherwise = divs (x:xs) ys o


    numerifier :: String -> [Int]
    numerifier input = let
        ww = map words (lines input)
        in
        (map . map) read ww

    main :: IO ()
    main = do
        input <- readFile "input2"
        let nums = numerifier input
        let diffs = map sethighlow nums
        print (sum diffs)

    mainb :: IO ()
    mainb = do
        input <- readFile "input2"
        let nums = numerifier input
        let diffs = map setdivs nums
        print (sum diffs)

