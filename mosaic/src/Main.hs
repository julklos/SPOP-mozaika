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


v :: Value
v = Just 5
simplest :: Table
simplest = [[C UNDECIDED (Just 1), C UNDECIDED (Just 1), C UNDECIDED (Just 1)],
           [C UNDECIDED Nothing,C UNDECIDED Nothing,C UNDECIDED Nothing],
           [C UNDECIDED (Just 1), C UNDECIDED (Just 1), C UNDECIDED (Just 1)]]
tableSimple :: Table
tableSimple = [[C UNDECIDED Nothing, C UNDECIDED Nothing, C UNDECIDED (Just 1), C UNDECIDED Nothing],
              [C UNDECIDED Nothing, C UNDECIDED (Just 0), C UNDECIDED (Just 2), C UNDECIDED Nothing],
              [C UNDECIDED Nothing,C UNDECIDED Nothing, C UNDECIDED (Just 1), C UNDECIDED Nothing]]
table :: Table
-- table = [[C UNDECIDED (Just 3), C WHITE Nothing, C UNDECIDED Nothing ],
--         [C UNDECIDED Nothing , C BLACK (Just 4) ,C UNDECIDED Nothing],
--         [C UNDECIDED Nothing , C UNDECIDED Nothing , C BLACK Nothing]]


table = [[C UNDECIDED Nothing, C UNDECIDED Nothing, C UNDECIDED (Just 5), C UNDECIDED Nothing, C UNDECIDED Nothing,C UNDECIDED Nothing, C UNDECIDED Nothing, C UNDECIDED (Just 5) , C UNDECIDED (Just 4), C UNDECIDED Nothing],
                [C UNDECIDED Nothing, C UNDECIDED (Just 5), C UNDECIDED Nothing,C UNDECIDED Nothing, C UNDECIDED (Just 6),C UNDECIDED Nothing,C UNDECIDED Nothing, C UNDECIDED (Just 5),C UNDECIDED Nothing,C UNDECIDED Nothing],
                [C UNDECIDED (Just 4), C UNDECIDED Nothing,C UNDECIDED (Just 2), C UNDECIDED Nothing,C UNDECIDED (Just 5),C UNDECIDED Nothing,C UNDECIDED Nothing,C UNDECIDED Nothing, C UNDECIDED (Just 4), C UNDECIDED (Just 4)],
                [C UNDECIDED Nothing, C UNDECIDED (Just 4),C UNDECIDED Nothing,C UNDECIDED Nothing,C UNDECIDED Nothing,C UNDECIDED Nothing, C UNDECIDED (Just 1),C UNDECIDED Nothing, C UNDECIDED Nothing,C UNDECIDED Nothing],
                [C UNDECIDED Nothing,C UNDECIDED Nothing,C UNDECIDED Nothing, C UNDECIDED (Just 1),C UNDECIDED Nothing,C UNDECIDED Nothing,  C UNDECIDED (Just 1), C UNDECIDED (Just 3), C UNDECIDED Nothing, C UNDECIDED (Just 5)],
                [C UNDECIDED Nothing,C UNDECIDED Nothing,C UNDECIDED Nothing,C UNDECIDED Nothing, C UNDECIDED (Just 3),C UNDECIDED Nothing,C UNDECIDED Nothing, C UNDECIDED (Just 3),  C UNDECIDED (Just 6),C UNDECIDED Nothing,C UNDECIDED Nothing],
                [C UNDECIDED Nothing, C UNDECIDED (Just 6), C UNDECIDED (Just 7), C UNDECIDED (Just 6),C UNDECIDED Nothing, C UNDECIDED (Just 4), C UNDECIDED Nothing,C UNDECIDED Nothing,C UNDECIDED Nothing,C UNDECIDED Nothing],
                [C UNDECIDED Nothing, C UNDECIDED (Just 3),C UNDECIDED Nothing,C UNDECIDED Nothing, C UNDECIDED (Just 7), C UNDECIDED (Just 7),C UNDECIDED Nothing,C UNDECIDED Nothing, C UNDECIDED (Just 3),  C UNDECIDED (Just 1)],
                [C UNDECIDED Nothing, C UNDECIDED (Just 1), C UNDECIDED (Just 3), C UNDECIDED Nothing,  C UNDECIDED (Just 8),C UNDECIDED Nothing,C UNDECIDED Nothing,C UNDECIDED Nothing, C UNDECIDED (Just 1), C UNDECIDED Nothing],
                [C UNDECIDED Nothing,C UNDECIDED Nothing,C UNDECIDED Nothing,C UNDECIDED Nothing,C UNDECIDED Nothing,C UNDECIDED Nothing, C UNDECIDED (Just 3),C UNDECIDED Nothing,C UNDECIDED Nothing,C UNDECIDED Nothing]]

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
tableRows :: Table -> Int
tableRows table = length table
tableCols :: Table -> Int
tableCols table = length (table !! 0)
                        
isSolvedRow [] = True
isSolvedRow (x:xs)
  | (getState x) == UNDECIDED = False
  | otherwise = isSolvedRow xs

isSolved [] = True
isSolved (x:xs)
  | (isSolvedRow x) == False = False
  | otherwise = isSolved xs                             

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
neighbourhoodsList x y table = [ (x+dx,y+dy) | dy <- [-1..1], dx <- [-1..1], x+dx>=0, y+dy>=0, x+dx<(tableRows table), y+dy<(tableCols table)]
positionsList :: Table -> [(Int, Int)]
positionsList table = [ (x,y) | y <- [0..(tableCols table)-1], x <- [0..(tableRows table) -1]]

solveOnePass :: Table -> [(Int, Int)] -> Table
solveOnePass table [] = table
solveOnePass table ((x,y):xs) = let val = getValue (byInd table x y)
                                in if isValue val then let changedTable = checkIfToColour table x y
                                                        in solveOnePass changedTable xs
                                    else solveOnePass table xs

solvePuzzle :: Table -> Table
solvePuzzle table = let pos = positionsList table
                        solvingTable = solveOnePass table pos
                        solved = isSolved solvingTable
                    in if solved then solvingTable
                        else solvePuzzle solvingTable
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

main :: IO ()
main = do putStrLn "Mosaic"
          -- filename <- getFileName
          -- puzzle <- readPuzzle filename
          -- let tablePuzzle = convertToTable puzzle
          print simplest
          let pos = positionsList simplest
          print pos
          let newtable = solveOnePass simplest pos
          print newtable
          let nextable = solveOnePass newtable pos
          print nextable
          let another = solveOnePass nextable pos
          print another
          let next = solveOnePass another pos
          print next

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
