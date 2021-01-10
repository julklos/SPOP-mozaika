module Main where
import System.IO (readFile)

data State = BLACK | WHITE | UNDECIDED deriving (Eq, Ord, Read, Show)
type Value = Maybe Int
data Cell = C State Value deriving Show 
type Table = [[Cell]]

fromJust :: Maybe Int -> Int
fromJust (Just a) = a

{------------------------------------------------------------------------
 -  @brief  Funkcja zwracająca stan z typu danych Cell
 -  @param  Cell  - komórka, z której pobieramy stan
 -  @retval State - stan danej komórki
 ------------------------------------------------------------------------}
getState :: Cell -> State
getState (C x y) = x

{------------------------------------------------------------------------
 -  @brief  Funkcja zwracająca wartość z typu danych Cell
 -  @param  Cell  - komórka, z której pobieramy wartość
 -  @retval Value - wartość danej komórki
 ------------------------------------------------------------------------}
getValue :: Cell -> Value
getValue (C x y) = y

{------------------------------------------------------------------------
 -  @brief  Funkcja sprawdzająca czy parametr ma wartość
 -  @param  Maybe Int 
 -  @retval Bool      - True jeśli ma wartość, w przeciwnym wypadku False
 ------------------------------------------------------------------------}
isValue :: Maybe Int -> Bool
isValue Nothing = False
isValue (Just _) = True

{------------------------------------------------------------------------
 -  @brief  Funkcja zwracająca komórkę znajdującą się pod odpowiednimi  
 -          indeksami w tablicy.
 -  @param  Table - tablica, z której pobierana jest komórka
 -  @param  Int   - indeks X
 -  @param  Int   - indeks Y
 -  @retval Cell  - odpowiednia komórka z tablicy
 ------------------------------------------------------------------------}
byInd :: Table -> Int -> Int -> Cell
byInd table row col = table !! row !! col

{------------------------------------------------------------------------
 -  @brief  Funkcja zastępująca element o podanym indeksie
 -  @param  Int - indeks elementu, który zastępujemy
 -  @param  a   - nowa wartość zastępowanego elementu
 -  @param  [a] - tablica, w której element zastępujemy
 -  @retval [a] - tablica z zamienionym elementem 
 ------------------------------------------------------------------------}
replace :: Int ->  a -> [a] -> [a]
replace _ _ [] = []
replace n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replace (n-1) newVal xs

{------------------------------------------------------------------------
 -  @brief  Funkcja zastępująca komórkę tablicy o podanym indeksie nową komórką
 -  @param  Table - tablica
 -  @param  Int   - indeks komórki tablicy (mozaiki)
 -  @param  Int   - indeks komórki tablicy (mozaiki)
 -  @param  Cell  - komórka do podstawienia w odpowiednie miejsce w tablicy
 -  @retval Table - tablica z podmienioną komórką 
 ------------------------------------------------------------------------}
replace_elem :: Table -> Int -> Int -> Cell -> Table
replace_elem xs row col x =
    let row_to_replace_in = xs !! row
        modified_row = replace col x row_to_replace_in
    in replace row modified_row xs


{------------------------------------------------------------------------
 -  @brief  Funkcja sprawdzająca poprawnośC rozwiązania pól mozaiki
 -  @param  Tablica     - sprawdzana tablica
 -  @param  [(Int,Int)] - tablica indeksów, które sprawdzamy
 -  @retval Bool        - True jeśli pola poprawne, False w przeciwnym wypadku
 ------------------------------------------------------------------------}
checkCorrectness :: Table -> [(Int,Int)]-> Bool
checkCorrectness _ [] = True
checkCorrectness table ((x,y):xs)
  | isElementCorrect table x y == False = False
  | otherwise = checkCorrectness table xs

{------------------------------------------------------------------------
 -  @brief  Funkcja sprawdzająca poprawność konkretnego pola mozaiki
 -  @param  Int   - sprawdzana tablica
 -  @param  Int   - tablica indeksów, które sprawdzamy
 -  @retval Bool  - True jeśli pole poprawne, False w przeciwnym wypadku
 ------------------------------------------------------------------------}
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
                                              

{------------------------------------------------------------------------
 -  @brief  Funkcja obliczająca liczbę wierszy w tablicy
 -  @param  Table   - tablica reprezentujaca mozaikę, 
 -  @retval Int     - liczba wierszy w tablicy
 ------------------------------------------------------------------------}
tableRows :: Table -> Int
tableRows table = length table

{------------------------------------------------------------------------
 -  @brief  Funkcja obliczająca liczbę kolumn w tablicy
 -  @param  Table   - tablica reprezentujaca mozaikę, 
 -  @retval Int     - liczba kolumn w tablicy
 ------------------------------------------------------------------------}
tableCols :: Table -> Int
tableCols table = length (table !! 0)
 
{------------------------------------------------------------------------
 -  @brief  Funkcja sprawdzająca czy dany wiersz został rozwiązany
 -          (czy nie ma ani jednego pola UNDECIDED)
 -  @param  []    - tablica do sprawdzenia 
 -  @retval Bool  - False jeśli w wieszu występuje choć jedno pole UNDECIDED 
 -                  True w przeciwnym wypadku
 ------------------------------------------------------------------------}        
isSolvedRow [] = True
isSolvedRow (x:xs)
  | (getState x) == UNDECIDED = False
  | otherwise = isSolvedRow xs

{------------------------------------------------------------------------
 -  @brief  Funkcja sprawdzająca czy cała mozaika została rozwiązana
 -          (czy nie ma ani jednego pola UNDECIDED)
 -  @param  []    - tablica do sprawdzenia 
 -  @retval Bool  - False jeśli występuje choć jedno pole UNDECIDED 
 -                  True w przeciwnym wypadku
 ------------------------------------------------------------------------}      
isSolved [] = True
isSolved (x:xs)
  | (isSolvedRow x) == False = False
  | otherwise = isSolved xs                             

{------------------------------------------------------------------------
 -  @brief  Funkcja obliczająca liczbę wystąpień danego stanu w tablicy
 -  @param  []    - tablica do sprawdzenia 
 -  @param  State - stan, którego wystąpienia chcemy zliczać 
 -  @retval Int   - liczba wystąpień stanu  
 ------------------------------------------------------------------------}      
countState :: Eq a => [a] -> a -> Int
countState [] find = 0
countState (x:xs) find 
  | find == x = 1 + (countState xs find)
  | otherwise = countState xs find

{------------------------------------------------------------------------
 -  @brief  Funkcja agregująca stany pól tablicy o podanych indeksach 
 -  @param  [(Int, Int)]  - tablica indeksów pól do sprawdzenia
 -  @param  Table         - tablica (mozaika)  
 -  @retval [State]       - tablica stanów pól o indeksach podanych w wywołaniu
 ------------------------------------------------------------------------}   
stateList :: [(Int, Int)] -> Table -> [State]
stateList [] _           = []
stateList ((x,y):xs) table = getState (byInd table x y): stateList xs table

{------------------------------------------------------------------------
 -  @brief  Funkcja generująca sąsiadów dla konkretnej komórki z tabeli 
 -  @param  Int         - indeks tabeli 
 -  @param  Int         - indeks tabeli
 -  @param  Table       - tablica (mozaika)  
 -  @retval [(Int,Int)] - tablica indeksów komórek sąsiadujących z komórką 
 -                        sprawdzaną
 ------------------------------------------------------------------------}   
neighbourhoodsList :: Int -> Int -> Table -> [(Int,Int)]
neighbourhoodsList x y table = [ (x+dx,y+dy) | dx <- [-1..1], dy <- [-1..1], x+dx>=0, y+dy>=0, x+dx<(tableRows table), y+dy<(tableCols table)]

{------------------------------------------------------------------------
 -  @brief  Funkcja znajdująca wszystkie komórki w tablicy 
 -  @param  Table       - tablica (mozaika)  
 -  @retval [(Int,Int)] - indeksy komórek znajdujących się w tablicy
 ------------------------------------------------------------------------}   
positionsList :: Table -> [(Int, Int)]
positionsList table = [ (x,y) |  y <- [0..(tableCols table)-1], x <- [0..(tableRows table) -1]]

{------------------------------------------------------------------------
 -  @brief  Funkcja przeprowadzająca jedna iteracje obliczeń na tablicy
 -  @param  Table       - tablica (mozaika)  
 -  @param  [(Int,Int)] - tablica indeksów komórek tablicy 
 -  @retval Maybe Table - tablica po jednej iteracji obliczeń
 ------------------------------------------------------------------------}   
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
{------------------------------------------------------------------------
 -  @brief  Funkcja tworząca dwie możliwe nowe rozwiązania poprzez podstawiene w komórce WHITE i BLACK
 -  @param  Table       - tablica (mozaika)  
 -  @param  Int         - indeks wiersza
 -  @param  Int         - indeks kolu,my
 -  @retval (Table, Table) - możliwe rozwiązania
 ------------------------------------------------------------------------}   
setTwoNextTables:: Table -> Int -> Int -> (Table, Table)
setTwoNextTables table x y = let cell = byInd table x y
                                 state = getState cell
                                 val = getValue cell
                              in if state == UNDECIDED then ( replace_elem table x y (C WHITE val), replace_elem table x y (C BLACK val))
                                 else (table, table)

{------------------------------------------------------------------------
 -  @brief  Funkcja rozwiązująca mozaikę
 -  @param  Table       - tablica z danymi o mazaice,
 -  @retval Maybe Table - pokolorowana tablica (mozaika)
 ------------------------------------------------------------------------}
solvePuzzle :: Table -> Maybe Table
solvePuzzle table = let pos = positionsList table
                    in solveOnePass table pos

{------------------------------------------------------------------------
 -  @brief  Funkcja wczytująca mozaike z pliku
 -  @param  String  - ścieżka do pliku z mozaiką, 
 ------------------------------------------------------------------------}
readPuzzle :: String -> IO [String]
readPuzzle filename = do
  contents <- readFile filename
  let puzzle = read contents :: [String]
  return puzzle

{------------------------------------------------------------------------
 -  @brief  Funkcja pobierająca ścieżkę do pliku, z którego ma zostać
 -          pobrana łamigłówka
 ------------------------------------------------------------------------}
getFileName :: IO String
getFileName = do putStrLn "Podaj nazwe pliku, z ktorgo ma zostac pobrana lamiglowka:"
                 filename <- getLine --użytkownik wprowadza nazwę
                 return filename

{------------------------------------------------------------------------
 -  @brief  Funkcja konwertująca znaki na liczby
 -  @param  [Char]  - tablica znaków, 
 -  @retval Int     - liczba
 ------------------------------------------------------------------------}
toInt :: [Char] -> Int
toInt x = read x :: Int

{------------------------------------------------------------------------
 -  @brief  Funkcja konwertująca dane tekstowe do struktury Table
 -  @param  [String]  - tablica stringów, z danymi tekstowymi określającymi
 -                    - daną mozaikę 
 -  @retval Table     - struktura Table, przechowująca informację dotyczące
 -                    - mozaiki
 ------------------------------------------------------------------------}
convertToTable:: [String] -> Table
convertToTable =  (map . map) parseCell
  where
    parseCell :: Char -> Cell
    parseCell '.' = C UNDECIDED Nothing
    parseCell ch = C UNDECIDED (Just (toInt [ch]))


{------------------------------------------------------------------------
 -  @brief  Funkcja wyświetlająca tablicę (mozaikę) w konsoli
 -  @param  Table  - tablica, którą chcemy wyświetlić
 ------------------------------------------------------------------------}
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
                    

{------------------------------------------------------------------------
 -  @brief  Funkcja główna
 ------------------------------------------------------------------------}
main :: IO ()
main = do putStrLn "Mosaic"
          filename <- getFileName
          puzzle <- readPuzzle filename
          print puzzle
          let convertedPuzzle = convertToTable puzzle
          case solvePuzzle convertedPuzzle of
              Just b -> printMap b
              _ -> putStrLn "Nie może być rozwiązane"
