module AdventOfCode.Sixteen where

    import Data.Char (isDigit)
    import qualified Data.Sequence as Seq

    start :: Seq.Seq Char
    start = Seq.fromList ['a'..'p']

    wordsBy :: (Char -> Bool) -> String -> [String]
    wordsBy p s =
        case dropWhile p s of
            "" -> []
            s' -> w : wordsBy p s''
                where (w, s'') = break p s'

    splitOn :: Char -> String -> [String]
    splitOn delimiter = wordsBy (== delimiter)

    findByValue :: Seq.Seq Char -> Char -> (Int, Char)
    findByValue s a = search 0
                where search i | Seq.index s i == a = (i, a)
                               | otherwise = search (i + 1)

    swap (i, a) (z, b) s = final
                    where afterOne = Seq.adjust (const a) z s
                          final = Seq.adjust (const b) i afterOne

    transform :: String -> Seq.Seq Char -> Seq.Seq Char
    transform ('p':x:_:y:xs) s = swap a b s
                        where a = findByValue s x
                              b = findByValue s y
    transform ('x':xs) s = let
                            [a, b] = map read $ splitOn '/' xs
                            i = (a, Seq.index s a)
                            k = (b, Seq.index s b)
                        in
                            swap i k s
    transform ('s':xs) s = let
                            range = read xs
                            tailers = Seq.drop (16 - range) s
                        in
                            Seq.take 16 $ tailers Seq.>< s

    solve1 xs s = foldr (fplit transform) s xs

    solve2 _ 0 s = s
    solve2 xs i s = solve2 xs (i - 1) (solve1 xs s)


    test :: Int -> [String] -> Seq.Seq Char -> Seq.Seq Char -> Int
    test i xs s s1 | i > 1000000000 = 0
                   | s == s1 && i > 0 = i
                   | otherwise = test (i + 1) xs (solve1 xs s) s1

    main :: IO ()
    main = do
        input <- readFile "input16"
        let ll = splitOn ',' (init input)
        let result = solve2 ll 10 start
--        let result = test 0 ll start start
        print result
