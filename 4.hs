module AdventOfCode.Four where

    valid :: [String] -> Bool
    valid [] = True
    valid (x:xs) | x `elem` xs = False
                 | otherwise = valid xs

    main :: IO ()
    main = do
        input <- readFile "input4"
        let ll = map words (lines input)
        let result = length $ filter valid ll
        print result

