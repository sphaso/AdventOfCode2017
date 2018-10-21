module AdventOfCode.Twelve where

    import Data.Char (isDigit)
    import Data.List (nub, sort)
    import qualified Data.Set as Set
    import qualified Data.Map.Strict as Map

    parse :: String -> (Int, [Int])
    parse s = let
                 (x:xs) = map (filter isDigit) (words s)
                 friends = filter (/="") xs
              in
                 (read x, map read friends)

    mappy :: [(Int, [Int])] -> Map.Map Int [Int]
    mappy = foldr (\(k, v) acc -> Map.insert k v acc) Map.empty

    dfs :: Int -> Map.Map Int [Int] -> Set.Set Int
    dfs k m = dfs' [k] Set.empty
        where
            dfs' [] s = s
            dfs' (x:xs) s | Set.member x s = dfs' xs s
                          | otherwise = dfs' ((m Map.! x) ++ xs) (Set.insert x s)

    solve2 graph = nub groups
            where groups = map (\x -> sort $ Set.toList $ dfs x graph) [0..1999]

    main :: IO ()
    main = do
        input <- readFile "input12"
        let graph = mappy $ map parse (lines input)
        -- let result = dfs 0 graph
        let result = solve2 graph
        print (length result)
