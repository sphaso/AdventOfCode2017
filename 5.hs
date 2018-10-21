module AdventOfCode.Five where

    import Data.Sequence (Seq, index, adjust, fromList, length)

    update n | n >= 3 = n - 1
             | otherwise = n + 1

    traverse'' seq pos step | inBnds = traverse'' inc next (step + 1)
                            | otherwise = step + 1
                            where curr = index seq pos
                                  inc = adjust update pos seq
                                  next = pos + curr
                                  inBnds = next >= 0 && next < Data.Sequence.length seq

    main :: IO ()
    main = do
        input <- readFile "input5"
        let ll = lines input
        let nn = map (\m -> read m :: Int) ll
        let aa = traverse'' (fromList nn) 0 0
        print aa
