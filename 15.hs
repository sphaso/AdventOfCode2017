module AdventOfCode.Fifteen where

    import Numeric (showIntAtBase)
    import Data.Char (intToDigit)
    import Data.Bits

    seedA :: Int
    seedA = 289

    seedB :: Int
    seedB = 629

    multA :: Int
    multA = 16807

    multB :: Int
    multB = 48271

    gen :: Int -> Int -> Int
    gen m x = (x*m) `mod` 2147483647

    generator :: Int -> Int -> [Int]
    generator seed mult = tail $ iterate (gen mult) seed

    clearUpBits :: Int -> Int
    clearUpBits n = n .&. 65535

    check :: [Int] -> [Int] -> [Bool]
    check = zipWith (\a b -> clearUpBits a == clearUpBits b)

    solve1 :: Int
    solve1 = length $ filter id (take 40000000 (check a b))
        where a = generator seedA multA
              b = generator seedB multB

    solve2 :: Int
    solve2 = length $ filter id (take 5000000 (check a b))
        where a = filter (\x -> x `mod` 4 == 0) $ generator seedA multA
              b = filter (\x -> x `mod` 8 == 0) $ generator seedB multB

