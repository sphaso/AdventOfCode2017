module AdventOfCode.Seven where

    import Data.Char (isAlpha, isDigit)
    import Data.List (nub, concatMap)

    parse :: String -> (String, Int, [String])
    parse s = let
                 (x:y:xs) = map (filter (\x -> isAlpha x || isDigit x)) (words s)
              in
                 (x, read y, filter (/="") xs)

    thr :: (String, Int, [String]) -> [String]
    thr (_, _, t) = t

    filterLeaves :: [(String, Int, [String])] -> [(String, Int, [String])]
    filterLeaves = filter ((/= []) . thr)

    supported :: [(String, Int, [String])] -> [String]
    supported w = nub $ concatMap thr w

    find :: String -> [(String, Int, [String])] -> (String, Int, [String])
    find name ll = head $ filter (\(n, _, _) -> n == name) ll

    findParent name ll = head $ filter (\(_, _, nn) -> name `elem` nn) ll

    lower :: [(String, Int, [String])] -> [String] -> String
    lower ((x, _, _):xs) sups | x `elem` sups = lower xs sups
                              | otherwise = x

    takeWeight (_, i, _) = i

    oddOne ((n, i):xs) | i `elem` map snd xs = oddOne xs
                       | otherwise = (n, i)

    sumBelow (n, i, []) _ = (n, i)
    sumBelow (n, i, xs) al = (n, i + s)
                    where children = map (`find` al) xs
                          s = sum $ map (snd . (`sumBelow` al)) children

    history a@(n, i, xs) al b | balanced = b
                              | otherwise = history (find h al) al sums
                    where children = map (`find` al) xs
                          sums = map (`sumBelow` al) children
                          (h, _) = oddOne sums
                          balanced = (==1) $ length $ nub $ map snd sums

    bottom = "hlqnsbe"

    solve1 ll = let
                    ww = filterLeaves $ map parse ll
                in
                    lower ww (supported ww)

    solve2 ll = let
                    ww = map parse ll
                in
                    history (find bottom ww) ww []

    main :: IO ()
    main = do
        input <- readFile "input7"
        let ll = lines input
        let low = solve2 ll
        print low
