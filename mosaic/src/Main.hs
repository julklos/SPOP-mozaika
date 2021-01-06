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

isValue :: Maybe Int -> Bool
isValue Nothing = False
isValue (Just _) = True

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

--pokoloruj na odpowiedni kolor
colourCells :: State -> [(Int, Int)] -> Table -> Table
colourCells _ [] table = table
colourCells color ((x,y):xs) table = let state = getState (byInd table x y)
                                     in if state == BLACK then colourCells color xs table
                                        else let val = getValue (byInd table x y)
                                                 modifiedTable = replace_elem table x y (C color val)
                                             in colourCells color xs modifiedTable

--sprawdz tablica jest poprawna
checkCorrectness :: Table -> [(Int,Int)]-> Bool
checkCorrectness _ [] = True
checkCorrectness table ((x,y):xs)
  | isElementCorrect table x y == False = False
  | otherwise = checkCorrectness table xs

isElementCorrect :: Table -> Int -> Int -> Bool
isElementCorrect table row col = let cell = byInd table row col
                                     cells_val = getValue cell
                                  in if not $ isValue cells_val then True
                                     else let val = fromJust cells_val
                                              neighbourhoods = neighbourhoodsList row col table
                                              states = stateList neighbourhoods table
                                              whites = countState states WHITE
                                              blacks = countState states BLACK
                                          in blacks <= val && whites <= length states - val
                                              


--oblicz liczbe wierszy
tableRows :: Table -> Int
tableRows table = length table
--oblicz liczbe kolumn
tableCols :: Table -> Int
tableCols table = length (table !! 0)

--sprawdz, czy rozwiazane per wiersz                      
isSolvedRow [] = True
isSolvedRow (x:xs)
  | (getState x) == UNDECIDED = False
  | otherwise = isSolvedRow xs
--sprawdz, czy rozwiazane- calosc
isSolved [] = True
isSolved (x:xs)
  | (isSolvedRow x) == False = False
  | otherwise = isSolved xs                             
--sprawdz ile wystepuje danego stanu
countState :: Eq a => [a] -> a -> Int
countState [] find = 0
countState (x:xs) find 
  | find == x = 1 + (countState xs find)
  | otherwise = countState xs find

--lita stanow
stateList :: [(Int, Int)] -> Table -> [State]
stateList [] _           = []
stateList ((x,y):xs) table = getState (byInd table x y): stateList xs table
--lista miejsc, ktore powinny zostac sprawdzone.. jeszcze nie wiem po co
shallBeCheckedList :: Int -> Int -> Table -> [(Int, Int)]
shallBeCheckedList x y table = [ (x+dx,y+dy) | dy <- [-2..2], dx <- [-2..2], x+dx>=0, y+dy>=0, x+dx<(tableRows table), y+dy<(tableCols table)]
-- wygeneruj liste sąsiadów komórki
neighbourhoodsList :: Int -> Int -> Table -> [(Int,Int)]
neighbourhoodsList x y table = [ (x+dx,y+dy) | dx <- [-1..1], dy <- [-1..1], x+dx>=0, y+dy>=0, x+dx<(tableRows table), y+dy<(tableCols table)]
--lista komorek w calej tablicy
positionsList :: Table -> [(Int, Int)]
positionsList table = [ (x,y) |  y <- [0..(tableCols table)-1], x <- [0..(tableRows table) -1]]
-- --rozwiaz jedna interacje
solveOnePass :: Table -> [(Int, Int)] -> Maybe Table
solveOnePass table [] = Just table
solveOnePass table ((x,y):xs) = let correct = checkCorrectness table (positionsList table)
                                    solved = isSolved table
                                in if not correct  then Nothing
                                   else if solved then Just table
                                   else let (table1, table2) = setTwoNextTables table x y
                                        in case solveOnePass table1 xs of
                                          Just res -> Just res
                                          Nothing  -> solveOnePass table2 xs

setTwoNextTables:: Table -> Int -> Int -> (Table, Table)
setTwoNextTables table x y = let cell = byInd table x y
                                 state = getState cell
                                 val = getValue cell
                              in if state == UNDECIDED then ( replace_elem table x y (C WHITE val), replace_elem table x y (C BLACK val))
                                 else (table, table)
solvePuzzle :: Table -> Maybe Table
solvePuzzle table = let pos = positionsList table
                    in solveOnePass table pos

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
-- znak z pliku to cyfry
toInt :: [Char] -> Int
toInt x = read x :: Int
-- konwersja stringa z pliku to tablicy
convertToTable:: [String] -> Table
convertToTable =  (map . map) parseCell
  where
    parseCell :: Char -> Cell
    parseCell '.' = C UNDECIDED Nothing
    parseCell ch = C UNDECIDED (Just (toInt [ch]))
-- wyswietlenie wynikow
printMap:: Table -> IO()
printMap = mapM_ (putStrLn . map toChar)
  where
    toChar :: Cell -> Char
    toChar cell = let val = getValue cell
                      state = getState cell
                  in if (state == UNDECIDED && (isValue val)) then toEnum ( (fromJust val) + 48)
                     else if state == UNDECIDED then '.'
                     else if state == WHITE then '_'
                     else 'X'
                    

main :: IO ()
main = do putStrLn "Mosaic"
          filename <- getFileName
          puzzle <- readPuzzle filename
          print puzzle
          let convertedPuzzle = convertToTable puzzle
          case solvePuzzle convertedPuzzle of
              Just b -> printMap b
              _ -> putStrLn "Nie może być rozwiązane"
          
          -- print simplest
          -- let pos = positionsList tableSimple
          -- print pos
          -- let newtable = solveOnePass tableSimple pos
          -- print newtable
          -- let nextable = solveOnePass newtable pos
          -- print nextable
          -- let another = solveOnePass nextable pos
          -- print another
          -- let next = solveOnePass another pos
          -- print next

          -- let solvedPuzzle = solvePuzzle 
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
