module AdventOfCode.Fourteen where

------------------------------------------------
    --ten
------------------------------------------------
    --

    import qualified Data.Sequence as Seq
    import Data.Char (ord, intToDigit)
    import Data.Bits (xor)
    import Numeric (showHex, readHex, showIntAtBase)
    import qualified Data.Set as Set

    salt     = [17, 31, 73, 47, 23]

    run input [] p s     = (input, p, s)
    run input (l:ls) p s = run new ls (p+l+s) (s+1)
                 where m = length input
                       poz = positions l p m
                       sublist = reverse $ map (Seq.index input) poz
                       new = updateSub input (zip poz sublist)

    updateSub seq [] = seq
    updateSub seq ((ix, v):xs) = updateSub new xs
                            where new = Seq.update ix v seq

    charToAscii :: String -> [Int]
    charToAscii s = map ord s ++ salt

    positions :: Int -> Int -> Int -> [Int]
    positions l p m = map (\x -> (p + x) `mod` m) [0..l-1]

    sparse input _ _ _ 0 = input
    sparse input l p s i = sparse next l np ns (i-1)
                where (next, np, ns) = run input l p s

    chunksOf :: Int -> Seq.Seq Int -> Seq.Seq (Seq.Seq Int) -> Seq.Seq (Seq.Seq Int)
    chunksOf i arr r | arr == Seq.empty = r
                     | otherwise = chunksOf i (Seq.drop i arr) (r Seq.|> Seq.take i arr)

    xorEmAll :: Seq.Seq Int -> Seq.Seq Int
    xorEmAll input = fmap xxor chunked
                where chunked = chunksOf 16 input Seq.empty
                      xxor chunk = foldr xor (Seq.index chunk 0) (Seq.drop 1 chunk)

    paddedShowHex x acc | length singleEx == 1 = "0" ++ singleEx ++ acc
                        | otherwise = singleEx ++ acc
                    where singleEx = showHex x ""

    hexify = foldr paddedShowHex ""

------------------

    ii = "stpzcrnm"

    rows = [ii ++ "-" ++ show i | i <- [0..127]]

    binary hash = showIntAtBase 2 intToDigit i ""
            where [(i, _)] = readHex hash

    hasher i = hexify . xorEmAll $ sparse (Seq.fromList [0..255]) (charToAscii i) 0 0 64

    countOnes :: String -> Int
    countOnes xs = length $ filter (=='1') xs

    lineGroups :: String -> Int -> Set.Set Int -> [Set.Set Int]
    lineGroups [] _ _ ss = filter (/= Set.empty) ss
    lineGroups ('1':xs) i c ss = lineGroups xs (i + 1) (Set.insert i c) ss
    lineGroups ('0':xs) i c ss = lineGroups xs (i + 1) Set.empty (ss ++ c)

    solve1 = sum $ map (countOnes . binary . hasher) rows
    solve2 = sum $ map (countRegions' . binary . hasher) rows
    solver = map (binary . hasher) rows

    -- convert to hex
    -- convert hex to bits
    -- count ups

