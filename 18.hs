module AdventOfCode.Eighteen where

    import qualified Data.Sequence as Seq
    import qualified Data.Map.Strict as Map
    import Data.Char (isDigit)

    traverse' :: Seq.Seq [String] -> Int -> Map.Map String Int -> Int -> Int
    traverse' cmds i m s = fromMaybe halt (traverse' cmds k newmap s2) s
                         where (k, newmap, s2, halt) = execute (Seq.index cmds i) i m s

    traverse'' :: Seq.Seq [String] -> Int -> Map.Map String Int -> [String] -> [String]
    traverse'' cmds i m s | rcv > snd = s
                          | otherwise = traverse'' cmds k newmap s2
                        where rcv = length $ filter (=="rcv") s
                              snd = length $ filter (=="snd") s
                              (k, newmap, s2) = execute' (Seq.index cmds i) i m s

    execute :: [String] -> Int -> Map.Map String Int -> Int -> (Int, Map.Map String Int, Int, Maybe Int)
    execute ["set", k, v] i m s | Map.member k m = (i + 1, Map.adjust (const value) k m, s, Nothing)
                                | otherwise = (i + 1, Map.insert k value m, s, Nothing)
                            where value = getValue v m
    execute ["add", k, v] i m s = (i + 1, Map.adjust (+value) k m, s, Nothing)
                            where value = getValue v m
    execute ["mul", k, v] i m s = (i + 1, Map.adjust (*value) k m, s, Nothing)
                            where value = getValue v m
    execute ["mod", k, v] i m s = (i + 1, Map.adjust (const $ curr `mod` value) k m, s, Nothing)
                            where value = getValue v m
                                  curr = m Map.! k
    execute ["jgz", k, v] i m s | curr > 0 = (i + steps, m, s, Nothing)
                                | otherwise = (i + 1, m, s, Nothing)
                            where steps = getValue v m
                                  curr = m Map.! k
    execute ["snd", k] i m s = (i + 1, m, m Map.! k, Nothing)
    execute ["rcv", k] i m s | curr > 0 = (i, m, s, Just s)
                             | otherwise = (i + 1, m, s, Nothing)
                            where curr = m Map.! k

    execute' :: [String] -> Int -> Map.Map String Int -> [String] -> (Int, Map.Map String Int, [String])
    execute' ["set", k, v] i m s | Map.member k m = (i + 1, Map.adjust (const value) k m, s)
                                 | otherwise = (i + 1, Map.insert k value m, s)
                              where value = getValue v m
    execute' ["add", k, v] i m s = (i + 1, Map.adjust (+value) k m, s)
                            where value = getValue v m
    execute' ["mul", k, v] i m s = (i + 1, Map.adjust (*value) k m, s)
                            where value = getValue v m
    execute' ["mod", k, v] i m s = (i + 1, Map.adjust (const $ curr `mod` value) k m, s)
                            where value = getValue v m
                                  curr = m Map.! k
    execute' ["jgz", k, v] i m s | curr > 0 = (i + steps, m, s)
                                 | otherwise = (i + 1, m, s)
                            where steps = getValue v m
                                  curr = m Map.! k
    execute' ["snd", k] i m s = (i + 1, m, s ++ ["snd"])
    execute' ["rcv", k] i m s = (i + 1, m, s ++ ["rcv"])

    getValue :: String -> Map.Map String Int -> Int
    getValue x m = fromMaybe (m Map.! x) (stringToInt x)

    stringToInt :: String -> Maybe Int
    stringToInt ('-':xs) = Just (negate read xs)
    stringToInt v@(x:xs) | isDigit x = Just (read v)
                         | otherwise = Nothing

    initMap = foldr (\k acc -> Map.insert [k] 0 acc) Map.empty ['a'..'z']

    solve1 instructions = traverse' (Seq.fromList instructions) 0 initMap 0
    solve2 instructions = traverse'' (Seq.fromList instructions) 0 initMap []

    main :: IO ()
    main = do
        input <- readFile "input18"
        let instructions = map words (lines input)
        let result = solve2 instructions
        print result
