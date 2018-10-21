module AdventOfCode.Nine where

    -- <>
    -- <random characters>
    -- <<<<>
    -- <{!>}>
    -- <!!>
    -- <!!!>>
    -- <{o"i!a,<{i<a>.

    parse :: String -> String -> String
    parse [] s = reverse s
    parse ('!':_:xs) s = parse xs s
    parse ('<':xs) s = parse (discardGarbage xs) s
    parse (',':xs) s = parse xs s
    parse (x:xs) s = parse xs (x:s)

    discardGarbage :: String -> String
    discardGarbage ('!':_:xs) = discardGarbage xs
    discardGarbage ('>':xs) = xs
    discardGarbage (_:xs) = discardGarbage xs

    parse2 :: String -> Int -> Int
    parse2 [] i = i
    parse2 ('!':_:xs) i = parse2 xs i
    parse2 ('<':xs) i = parse2 (discardGarbage xs) (countGarbage xs i)
    parse2 (_:xs) i = parse2 xs i

    countGarbage ('!':_:xs) i = countGarbage xs i
    countGarbage ('>':xs) i = i
    countGarbage (_:xs) i = countGarbage xs (i+1)

    -- {} == 1.
    -- {{{}}} == 1 + 2 + 3 = 6.
    -- {{}{}} == 1 + 2 + 2 = 5.
    -- {{{}{}{{}}}} == 1 + 2 + 3 + 3 + 3 + 4 = 16.
    -- {{}{}{}{}} == 1 + 2 + 2 + 2 + 2 = 9.

    score :: String -> Int -> Int -> Int
    score [] 0 r = r
    score ('{':xs) d r = score xs (d+1) r
    score ('}':xs) d r = score xs (d-1) (r+d)

    main :: IO ()
    main = do
        input <- readFile "input9"
        -- let sanitized = parse (init input) []
        -- let s = score sanitized 0 0
        let count = parse2 (init input) 0
        print count
