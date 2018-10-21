module AdventOfCode.Six where

    import Data.Sequence (Seq, index, adjust, fromList, length)
    import qualified Data.Set as Set

    blocks = fromList [11, 11, 13, 7, 0, 15, 5, 5, 4, 4, 1, 1, 7, 1, 15, 11]

    spread seq 0 _ = seq
    spread seq i p = spread news (i - 1) next
                where next = (p + 1) `mod` (Data.Sequence.length seq)
                      news = adjust (+1) p seq

    max' seq i | any (\x -> x > curr) seq = max' seq (i + 1)
               | otherwise = (i, curr)
            where curr = index seq i

    move curr prev | Set.member curr prev = (curr, Set.size prev)
                   | otherwise = move next (Set.insert curr prev)
                   where (i, maximi) = max' curr 0
                         update = adjust (const 0) i curr
                         next = spread update maximi ((i + 1) `mod` (Data.Sequence.length curr))

    run = move blocks Set.empty
    run2 = let
            (newStart, _) = move blocks Set.empty
           in
            move newStart Set.empty

    ex = move (fromList [0, 2, 7, 0]) Set.empty
    ex2 = move (fromList [2, 4, 1, 2]) Set.empty
