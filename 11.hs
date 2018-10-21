module AdventOfCode.Eleven where

    wordsBy :: (Char -> Bool) -> String -> [String]
    wordsBy p s =
        case dropWhile p s of
            "" -> []
            s' -> w : wordsBy p s''
                where (w, s'') = break p s'

    splitOn :: Char -> String -> [String]
    splitOn delimiter = wordsBy (== delimiter)

    move :: String -> (Int, Int, Int)
    move "n" = (0, 1, -1)
    move "ne" = (1, 0, -1)
    move "nw" = (-1, 1, 0)
    move "s" = (0, -1, 1)
    move "se" = (1, -1, 0)
    move "sw" = (-1, 0, 1)

    transform :: [String] -> [(Int, Int, Int)]
    transform = map move

    sumTwoCoords :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
    sumTwoCoords (a, b, c) (x, y, z) = (a+x, b+y, c+z)

    summa :: [(Int, Int, Int)] -> (Int, Int, Int)
    summa = foldr (sumTwoCoords) (0, 0, 0)

    distance :: (Int, Int, Int) -> Int
    distance (x, y, z) = maximum [abs(x), abs(y), abs(z)]

    solve1 = distance . summa . transform

    solve2 :: [String] -> Int
    solve2 a = maximum $ map distance $ scanl sumTwoCoords (0, 0, 0) $ transform a

    main :: IO ()
    main = do
        input <- readFile "input11"
        let directions = splitOn ',' (init input)
        let result = solve2 directions
        print result
