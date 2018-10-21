module AdventOfCode.Three where

    bucket :: (Integral n) => n -> n
    bucket 1 = 1
    bucket n | n*n > 368078 = div n 2
             | otherwise = bucket (n + 2)
