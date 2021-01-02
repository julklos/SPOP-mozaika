module Main where
import System.IO (readFile)

data State = BLACK | WHITE | UNDECIDED deriving (Eq, Ord, Read, Show)
type Value = Maybe Int
data Cell = C State Value deriving Show 
type Table = [[Cell]]

getState :: Cell -> State
getState (C x y) = x

getValue :: Cell -> Value
getValue (C x y) = y

fromJust :: Maybe Int -> Int
fromJust (Just a) = a


-- type Table = [[Cell]]
-- type Cell = Value

byInd :: Table -> Int -> Int -> Cell
byInd table row col = table !! row !! col

v :: Value
v = Just 5

table :: Table
table = [[C UNDECIDED (Just 5), C UNDECIDED Nothing ],
        [C UNDECIDED Nothing , C UNDECIDED Nothing ]]
-- table = [[(UNDECIDED, Nothing),(UNDECIDED, Nothing), (UNDECIDED,(Just 5)),(UNDECIDED, Nothing),(UNDECIDED, Nothing),(UNDECIDED, Nothing),(UNDECIDED, Nothing),(UNDECIDED,5), (UNDECIDED,4), (UNDECIDED,Nothing)],
--                 [(UNDECIDED,Nothing),(UNDECIDED,5), (UNDECIDED, Nothing),(UNDECIDED, Nothing),(UNDECIDED,6),(UNDECIDED, Nothing),(UNDECIDED, Nothing),(UNDECIDED,5),(UNDECIDED, Nothing),(UNDECIDED, Nothing)],
--                 [(UNDECIDED,4), (UNDECIDED,Nothing ), (UNDECIDED,2), (UNDECIDED,Nothing), (UNDECIDED,5),(UNDECIDED, Nothing),(UNDECIDED, Nothing),(UNDECIDED, Nothing),(UNDECIDED,4),(UNDECIDED,4)],
--                 [(UNDECIDED,Nothing ), (UNDECIDED,4),(UNDECIDED, Nothing),(UNDECIDED, Nothing),(UNDECIDED, Nothing),(UNDECIDED, Nothing),(UNDECIDED,1),(UNDECIDED, Nothing),(UNDECIDED, Nothing),(UNDECIDED,Nothing )],
--                 [(UNDECIDED, Nothing),(UNDECIDED, Nothing),(UNDECIDED,Nothing),(UNDECIDED,1),(UNDECIDED, Nothing),(UNDECIDED, Nothing), (UNDECIDED, 1), (UNDECIDED,3), (UNDECIDED,Nothing ),(UNDECIDED,5)],
--                 [(UNDECIDED, Nothing),(UNDECIDED, Nothing),(UNDECIDED, Nothing),(UNDECIDED, Nothing), (UNDECIDED,3),(UNDECIDED, Nothing),(UNDECIDED, Nothing),(UNDECIDED,3), (UNDECIDED,6),(UNDECIDED, Nothing),(UNDECIDED, Nothing)],
--                 [(UNDECIDED,Nothing), (UNDECIDED,6), (UNDECIDED,7), (UNDECIDED,6),(UNDECIDED,Nothing),(UNDECIDED,4), (UNDECIDED, Nothing),(UNDECIDED, Nothing),(UNDECIDED, Nothing),(UNDECIDED, Nothing)],
--                 [(UNDECIDED,Nothing ),(UNDECIDED,3),(UNDECIDED, Nothing),(UNDECIDED, Nothing),(UNDECIDED,7),(UNDECIDED,7),(UNDECIDED, Nothing),(UNDECIDED, Nothing),(UNDECIDED,3), (UNDECIDED,1)],
--                 [(UNDECIDED,Nothing ), (UNDECIDED,1), (UNDECIDED,3), (UNDECIDED,Nothing), (UNDECIDED,8),(UNDECIDED, Nothing),(UNDECIDED, Nothing),(UNDECIDED, Nothing),(UNDECIDED,1), (UNDECIDED, Nothing)],
--                 [(UNDECIDED, Nothing),(UNDECIDED, Nothing),(UNDECIDED, Nothing),(UNDECIDED, Nothing),(UNDECIDED, Nothing),(UNDECIDED, Nothing),(UNDECIDED,3),(UNDECIDED, Nothing),(UNDECIDED, Nothing),(UNDECIDED,Nothing)]]
--pobrany tekst z łamigłowką przekonwertuj do tablicy
-- convertToTable :: [Char] -> [IO()]
-- convertToTable (x:xs) row table = setValue x : convertToTable xs row table
-- -- convertToTable [x] = print x
-- convertToTable ("[":xs)   _   _     = convertToTable xs [] []
-- convertToTable ("\"": xs) row table = convertToTable xs row table
-- convertToTable (",":xs)   row table = convertToTable xs row (table++row)
-- convertToTable ("]":xs)   row table = table++row
-- convertToTable (".":xs)   row table = convertToTable xs (row++[]) tables
-- convertToTable (x:xs)     row table = let val = Cell(UNDECIDED, x)
--                                       in convertToTable xs (row++ val) table
-- setValue x row = case x of
--                     "\""
-- convertToTable (x:xs) row  = setValue x row : convertToTable xs row
 
-- pobierz łamigłówkę z pliku
readPuzzle :: String -> IO [String]
readPuzzle filename = do
  contents <- readFile filename
  let puzzle = read contents :: [String]
  return puzzle
-- wprowadź ścieżkę do pliku, z którego ma zostać pobrana łamigłówka
getFileName :: IO String
getFileName = do putStrLn "Podaj nazwę pliku, z którgo ma zostać pobrana łamigłówka:"
                 filename <- getLine --użytkownik wprowadza nazwę
                 return filename
nth _ []       = Nothing
nth 1 (x : _)  = Just x
nth n (_ : xs) = nth (n - 1) xs
main :: IO ()
main = do putStrLn "Mosaic"
          -- filename <- getFileName
          -- puzzle <- readPuzzle filename
          -- let tablePuzzle = convertToTable puzzle 
          let val =  table !! 0 !! 0
          print (getValue val)
          print( byInd table 0 0)
          --solvedPuzzle <- solve tablePuzzle
          -- print puzzle
