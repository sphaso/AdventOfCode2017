module AdventOfCode.Nineteen where

    import qualified Data.Sequence as Seq

    data Directions = U | D | L | R deriving (Eq)

    findStartingPoint :: String -> Int -> Int
    findStartingPoint (x:xs) i | x == '|' = i
                               | otherwise = findStartingPoint xs (i + 1)

    move :: Seq.Seq String -> (Int, Int) -> Directions -> [String] -> Int -> ([String], Int)
    move seqs (row, col) direction alphabet steps | curr == '+' = move seqs (follow (row, col) nextDir) nextDir alphabet (steps + 1)
                                                  | curr `elem` ['A'..'Z'] = move seqs next direction (alphabet ++ [[curr]]) (steps + 1)
                                                  | curr /= ' ' = move seqs next direction alphabet (steps + 1)
                                                  | otherwise = (alphabet, steps)
                                            where curr = takePoint seqs (row, col)
                                                  next = follow (row, col) direction
                                                  nextDir = changeDirection seqs (row, col) direction

    takePoint :: Seq.Seq String -> (Int, Int) -> Char
    takePoint seqs (row, col) = Seq.index seqs row !! col

    follow :: (Int, Int) -> Directions -> (Int, Int)
    follow (row, col) U = (row - 1, col)
    follow (row, col) D = (row + 1, col)
    follow (row, col) L = (row, col - 1)
    follow (row, col) R = (row, col + 1)

    changeDirection :: Seq.Seq String -> (Int, Int) -> Directions -> Directions
    changeDirection seqs (row, col) d | d `elem` [U, D] && right /= ' ' = R
                                      | d `elem` [U, D] = L
                                      | up /= ' ' = U
                                      | otherwise = D
                            where right = takePoint seqs (row, col + 1)
                                  up = takePoint seqs (row - 1, col)

    solve1 :: [String] -> ([String], Int)
    solve1 rows = move seq startingPoint D [] 0
            where seq = Seq.fromList rows
                  startingPoint = (0, findStartingPoint (head rows) 0)

    main :: IO ()
    main = do
        input <- readFile "input19"
        let rows = lines input
        let result = solve1 rows
        print result
