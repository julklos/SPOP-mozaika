module Main where
import System.IO (readFile)

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
          filename <- getFileName
          puzzle <- readPuzzle filename
          print (show $ puzzle)
