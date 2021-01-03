module Main
    ( FieldState (..)
    , FieldNumber (..)
    , Field (..)
    , readPuzzleRaw
    , parsePuzzle
    , prettyPrint
    , solve
    , main
    ) where

data FieldState = Unresolved | White | Black deriving (Eq, Show)

type FieldNumber = Maybe Int

type Field = (FieldState, FieldNumber)

type Row = [Field]
type Triple a = (a, a, a)

type Board = [Row]

empty :: Field
empty = (Unresolved, Nothing)

unresolved :: FieldNumber -> Field
unresolved = (,) Unresolved

-- |Reads puzzle as a list of String where
-- each field is represented either by a digit,
-- if the field contained a digit or "." character
-- from the provided path.
readPuzzleRaw :: String -> IO [String]
readPuzzleRaw = fmap read <$> readFile 

-- |Parses puzzle from format described in
-- 'readPuzzleRaw' function to a 'Board' type.
-- Additionaly adds 2 rows and 2 columns full of white
-- fields at the edges of the board.
parsePuzzle :: [String] -> Board
parsePuzzle = addBarriers . (map . map) parseField
  where
    parseField :: Char -> Field
    parseField '.' = empty
    parseField ch = unresolved . Just . digitFromChar $ ch

    digitFromChar :: Char -> Int
    digitFromChar = subtract 48 . fromEnum

    barrier :: Field
    barrier = (White, Nothing)
    
    addBarriers :: Board -> Board
    addBarriers xs =
      let rowLen = length (head xs)
          barrRow = replicate (rowLen+2) barrier 
      in  barrRow : map addBarriersRows xs ++ [barrRow]

    addBarriersRows :: Row -> Row
    addBarriersRows x = barrier : x ++ [barrier]

-- | Prints the 'Board' to the stdout.
-- If the field is unresolved then the number in it
-- is printed or "x" if it doesn't have it.
-- Otherwise "_" is printed for 'White' and '#' for black.
prettyPrint :: Board -> IO ()
prettyPrint = mapM_ (putStrLn . map toChar)
  where
    toChar :: Field -> Char
    toChar (Unresolved, Nothing) = 'x'
    toChar (Unresolved, Just x) = toEnum $ x + 48
    toChar (White, _) = '_'
    toChar (Black, _) = '#'

-- |Checks if all fields in the board are either
-- White or Black.
completed :: Board -> Bool
completed = all $ and . map fieldCompleted
  where
    fieldCompleted :: Field -> Bool
    fieldCompleted (Unresolved ,_) = False
    fieldCompleted _ = True 

-- |Folds a matrix passing 3x3 squares to a
-- folding function. It passes every 3x3 square
-- possible to create in the matrix (squares will overlap)
-- going by columns and then rows.
foldrSquares :: ([[a]] -> b -> b) -> b -> [[a]] -> b 
foldrSquares f init b = foldr f init squares'
  where
    tripleToList :: (a, a, a) -> [a]
    tripleToList (x, y, z) = [x, y, z]

    triplets :: [a] -> [(a, a, a)]
    triplets a = zip3 a (drop 1 a) (drop 2 a)
    
    getSquares :: [[a]] -> [Triple (Triple a)]
    getSquares [x, y, z] = zip3 (triplets x) (triplets y) (triplets z)
    
    rowTriplets = map tripleToList $ triplets b
    squares = concatMap getSquares rowTriplets
    squares' = (map . map) tripleToList $ map tripleToList squares

-- |Checks if the board is in the legal state.
-- Boards is in legal state if the resolved tiles
-- are filled according to the game rules which
-- means unresolved tiles do not contribute to the
-- legality of the board.
legal :: Board -> Bool
legal = foldrSquares squareLegal True
  where
    squareLegal :: Board -> Bool -> Bool
    squareLegal _ False = False
    squareLegal [_, [_, (_, Nothing), _], _] acc = acc
    squareLegal square@[_, [_, (_, Just dig), _], _] _ =
      let (_, wCount, bCount) = countFields $ concat square
      in bCount <= dig && wCount <= 9 - dig 
 
    countFields :: [Field] -> (Int, Int, Int)
    countFields [] = (0, 0, 0)
    countFields ((Unresolved, _):xs) = (1, 0, 0) `add3` countFields xs
    countFields ((White, _):xs) = (0, 1, 0) `add3` countFields xs
    countFields ((Black, _):xs) = (0, 0, 1) `add3` countFields xs

    add3 :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
    add3 (x1, y1, z1) (x2, y2, z2) = (x1+x2, y1+y2, z1+z2)

-- |Sets column 'c' and row 'r' indexed from 0
-- in the board 'board' to 'FieldState' state
-- without changing the number value. 
set :: Board -> Int -> Int -> FieldState -> Board
set board c r state =
  take r board 
  ++ [take c (board !! r) ++ ((state, num) : drop (c+1) (board !! r))]
  ++ drop (r+1) board
  where
    (_, num) = board !! r !! c

-- |Solves given board returning solved board
-- or `Nothing` if the board couldn't be solved.
-- An assumption is made that the input board is
-- solvable, in a legal state and has auxilary
-- rows and columns filled with White fields around
-- board edges.
solve :: Board -> Maybe Board
solve board = solveRec 1 1 board
  where
    solveRec :: Int -> Int -> Board -> Maybe Board
    solveRec c r board
        | not $ legal board = Nothing
        | completed board && legal board = Just board
        | otherwise =
            let (c', r') = if c + 1 == length board then (0, r+1) else (c+1, r)
                (board', board'') = case board !! r !! c of 
                    (Unresolved, _) -> (set board c r White, set board c r Black)
                    _ -> (board, board)
            in case solveRec c' r' board' of
                Just b -> Just b
                Nothing -> solveRec c' r' board''

main :: IO ()
main = do
    path <- getLine
    raw <- readPuzzleRaw path
    let puzz = parsePuzzle raw
    case solve puzz of
        Just b -> prettyPrint b
        Nothing -> putStrLn "the board bould not be solved" 