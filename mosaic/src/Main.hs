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

replace :: Int ->  a -> [a] -> [a]
replace _ _ [] = []
replace n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replace (n-1) newVal xs

replace_elem :: Table -> Int -> Int -> Cell -> Table
replace_elem xs row col x =
    let row_to_replace_in = xs !! row
        modified_row = replace col x row_to_replace_in
    in replace row modified_row xs


v :: Value
v = Just 5

table :: Table
table = [[C UNDECIDED (Just 3), C WHITE Nothing, C UNDECIDED Nothing ],
        [C UNDECIDED Nothing , C BLACK (Just 4) ,C UNDECIDED Nothing],
        [C UNDECIDED Nothing , C UNDECIDED Nothing , C BLACK Nothing]]
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
colourCells :: State -> [(Int, Int)] -> Table -> Table
colourCells _ [] table = table
colourCells color ((x,y):xs) table = let state = getState (byInd table x y)
                                     in if state /= UNDECIDED then colourCells color xs table
                                        else let val = getValue (byInd table x y)
                                                 modifiedTable = replace_elem table x y (C color val)
                                             in colourCells color xs modifiedTable

checkIfToColour :: Table -> Int -> Int -> Table
checkIfToColour table row col = let neighbourhoods = neighbourhoodsList row col table
                                    states = stateList neighbourhoods table
                                    whites = countState states WHITE
                                    blacks = countState states BLACK
                                    unds = countState states UNDECIDED
                                    val = fromJust (getValue (byInd table row col))
                                    not_val = length states - val
                                  in if val - blacks == unds then colourCells BLACK neighbourhoods table
                                     else if not_val - whites == unds then colourCells WHITE neighbourhoods table
                                          else table
                                    
                                

countState :: Eq a => [a] -> a -> Int
countState [] find = 0
countState (x:xs) find 
  | find == x = 1 + (countState xs find)
  | otherwise = countState xs find

stateList :: [(Int, Int)] -> Table -> [State]
stateList [] _           = []
stateList ((x,y):xs) table = getState (byInd table x y): stateList xs table
-- wygeneruj liste sąsiadów komórki
neighbourhoodsList :: Int -> Int -> Table -> [(Int,Int)]
neighbourhoodsList x y table = [ (x+dx,y+dy) | dy <- [-1..1], dx <- [-1..1], x+dx>=0, y+dy>=0, x+dx<(length table), y+dy<(length (table !! 0))]
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
          print table
          let newtable = checkIfToColour table 0 0
          print newtable
          -- let cols = length (table !! 0)
          -- let rows = length table
          -- let val =  table !! 1 !! 0
          -- print (getValue val)
          -- print( byInd table 1 0)
          -- let newtable = replace_elem table 1 0 (C UNDECIDED (Just 3))
          -- print (byInd table 1 0)
          -- print (byInd newtable 1 0)
          -- print (rows, cols)
          -- let neighbourhoods = neighbourhoodsList 1 1 table
          -- let states = stateList neighbourhoods table
          -- let whites = countState states WHITE
          -- let blacks = countState states BLACK
          -- let unds = countState states UNDECIDED
          -- let val = fromJust (getValue (byInd table 1 1))
          -- let not_val = length states - val
          -- print (val, not_val, length states)
          --solvedPuzzle <- solve tablePuzzle
          -- print puzzle
