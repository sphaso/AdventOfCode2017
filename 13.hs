module AdventOfCode.Thirteen where

    import Data.Char (isDigit)
    import Data.List (find)

    data Direction = U | D deriving (Show)

    parse :: String -> (Int, Int, Int, Direction)
    parse s = let
                 (x:y:xs) = map (filter isDigit (words s))
              in
                 (read x, read y, 0, D)

    move :: (Int, Int, Int, Direction) -> (Int, Int, Int, Direction)
    move (ix, d, c, U) | c == 0 = (ix, d, 1, D)
                       | otherwise = (ix, d, c - 1, U)
    move (ix, d, c, D) | c == d -1 = (ix, d, c - 1, U)
                       | otherwise = (ix, d, c + 1, D)

    dmg (ix, d, 0, U) = ix * d
    dmg _ = 0

    run :: Int -> [(Int, Int, Int, Direction)] -> Int -> Int
    run _ [] score = score
    run pole walls@(x:xs) score | pole == ix = run (pole + 1) (map move xs) (score + dmg x)
                                | otherwise = run (pole + 1) (map move walls) score
                            where (ix, _, _, _) = x

    hit (0, _, 0, _) = True
    hit (ix, d, 0, U) = True
    hit _ = False

    caught :: Int -> [(Int, Int, Int, Direction)] -> Bool
    caught _ [] = False
    caught pole walls@(x:xs) | pole == ix && hit x = True
                             | pole == ix = caught (pole + 1) (map move xs)
                             | otherwise = caught (pole + 1) (map move walls)
                            where (ix, _, _, _) = x


    dance walls 0 = walls
    dance walls i = dance (map move walls) (i - 1)

    runn walls = find (not . snd) $ map (\i -> (i, caught 0 (dance walls i))) [0..]

    main :: IO ()
    main = do
        input <- readFile "input13"
        let li = lines input
        let walls = map parse li
        -- let result = run 0 walls 0
        let result = runn walls
        print result

    test = do
        let walls = map parse ["0: 3", "1: 2", "4: 4", "6: 4"]
        -- let result = run 0 walls 0
        let result = runn walls
        print result
