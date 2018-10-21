module AdventOfCode.Seventeen where

    import qualified Data.Sequence as Seq

    merry ll i 50000000 r = Seq.index ll (1 + (i + r) `mod` Seq.length ll)
    merry ll i v r = merry updated nextup (v + 1) r
            where len = Seq.length ll
                  nextup = ((i + r) `mod` len) + 1
                  firstTrunk = Seq.take nextup ll
                  secondTrunk = Seq.drop nextup ll
                  updated = (firstTrunk Seq.|> v) Seq.>< secondTrunk

    solve1 = merry (Seq.fromList [0,1]) 1 2 366
    test l i v = merry l v i 3

