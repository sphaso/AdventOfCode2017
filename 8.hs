module AdventOfCode.Eight where

    import qualified Data.Map.Strict as Map

    parse :: String -> (String, String, Int, String, String, Int)
    parse str = (s, op, read by, s2, cond, read v)
            where [s, op, by, _, s2, cond, v] = words str

    getSymbols :: [(String, String, Int, String, String, Int)] -> [String] -> [String]
    getSymbols [] acc = acc
    getSymbols ((x, _, _, y, _, _):xs) acc = getSymbols xs (x:y:acc)

    insertAllSymbols [] m = m
    insertAllSymbols (x:xs) m | x `Map.member` m = insertAllSymbols xs m
                              | otherwise = insertAllSymbols xs (Map.insert x 0 m)

    run [] m = m
    run ((s, op, by, s2, cond, val):xs) m | condition (m Map.! s2) cond val = run xs (compute s op by m)
                                          | otherwise = run xs m


    run2 :: [(String, String, Int, String, String, Int)] -> Map.Map String Int -> Int -> Int
    run2 [] m mm = mm
    run2 ((s, op, by, s2, cond, val):xs) m mm | condition (m Map.! s2) cond val = run2 xs up (max mm (biggestVal up))
                                              | otherwise = run2 xs m mm
                                            where up = compute s op by m

    compute key "inc" by m = Map.adjust (+by) key m
    compute key "dec" by m = Map.adjust (+ (by * (-1))) key m

    condition :: Int -> String -> Int -> Bool
    condition sval "==" val = sval == val
    condition sval ">" val = sval > val
    condition sval ">=" val = sval >= val
    condition sval "<" val = sval < val
    condition sval "<=" val = sval <= val
    condition sval "!=" val = sval /= val

    biggestVal :: Map.Map String Int -> Int
    biggestVal = Map.foldr max 0

    main :: IO ()
    main = do
        input <- readFile "input8"
        let ll = map parse (lines input)
        let m = insertAllSymbols (getSymbols ll []) Map.empty
        -- let finalMap = run ll m
        -- let result = biggestVal finalMap
        let result = run2 ll m 0
        print result
