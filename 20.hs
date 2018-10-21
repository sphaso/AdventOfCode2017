module AdventOfCode.Twenty where

    import Data.List (sortBy)

    wordsBy :: (Char -> Bool) -> String -> [String]
    wordsBy p s =
        case dropWhile p s of
            "" -> []
            s' -> w : wordsBy p s''
                where (w, s'') = break p s'

    splitOn :: Char -> String -> [String]
    splitOn delimiter = wordsBy (== delimiter)

    parse :: String -> ([Int], [Int], [Int])
    parse s = ([px, py, pz], [vx, vy, vz], [ax, ay, az])
        where [px, py, pz, vx, vy, vz, ax, ay, az] = map read $ splitOn ',' s

    update :: (Int, ([Int], [Int], [Int])) -> (Int, ([Int], [Int], [Int]))
    update (i, (p, v, a)) = (i, (newp, newv, a))
                where newv = zipWith (+) v a
                      newp = zipWith (+) newv p

    removeDuplicate :: [(Int, ([Int], [Int], [Int]))] -> [(Int, ([Int], [Int], [Int]))] -> [(Int, ([Int], [Int], [Int]))]
    removeDuplicate [] b = b
    removeDuplicate a@(x:xs) b | length new < length xs = removeDuplicate new b
                               | otherwise = removeDuplicate xs (x:b)
                        where (i, (p, _, _)) = x
                              new = filter (\(k, (q, _, _)) -> p /= q) xs

    runThrough :: [(Int, ([Int], [Int], [Int]))] -> [(Int, ([Int], [Int], [Int]))] -> [(Int, ([Int], [Int], [Int]))]
    runThrough a b | length a == length b = a
                   | otherwise = runThrough (removeDuplicate (map update a) []) a

    distance :: [Int] -> Int
    distance p = sum $ map abs p

    roulette :: Int -> [(Int, ([Int], [Int], [Int]))] -> [(Int, ([Int], [Int], [Int]))]
    roulette 0 d = d
    roulette i d = roulette (i - 1) (map update d)

    order :: (Int, ([Int], [Int], [Int])) -> (Int, ([Int], [Int], [Int])) -> Ordering
    order (i, (p, _, _)) (k, (q, _, _)) | dist_p > dist_q = GT
                                        | dist_p < dist_q = LT
                                        | otherwise = EQ
                                where dist_p = distance p
                                      dist_q = distance q


    solve1 :: [(Int, ([Int], [Int], [Int]))] -> (Int, ([Int], [Int], [Int]))
    solve1 c = minimumBy order (roulette 1000 c)

    solve2 :: [(Int, ([Int], [Int], [Int]))] -> Int
    solve2 c = length $ runThrough c []

    prepare = zipWith (\i e -> (i, e)) [0..]

    main :: IO ()
    main = do
        input <- readFile "input20"
        let dd = map parse (lines input)
        let result = solve2 (prepare dd)
        print result
