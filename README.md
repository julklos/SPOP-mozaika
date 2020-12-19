# SPOP-mozaika

Projekt z przedmiotu SPOP.

 W ramach zadania należy napisać aplikację rozwiązującą łamigłówkę „Mozaika”. Łamigłówka polega na zamalowywaniu pól planszy według podanych cyfr. Cyfra (0–9) na danym polu oznacza ile sąsiednich pól, włącznie z polem z cyfrą, powinno być zamalowanych.

Algorytm poszukiwania rozwiązania łamigłówki jest częścią zadania. Po uruchomieniu aplikacjapowinna spytać użytkownika o nazwę pliku wejściowego z łamigłówką. Taki plik zawiera opisłamigłówki w postaci:["pola 1-go wiersza", "pola 2-go wiersza", ..., "pola n-tego wiersza"]Zawartość kolejnych pól wiersza jest przedstawiona napisem złożonym z cyfr (0–9) i symbolukropki, gdzie kropka oznacza pole puste. Plik wejściowy dla pokazanej wyżej łamigłówki mógł-by mieć postać: ["..5....54.",".5..6..5..","4.2.5...44",".4....1...","...1..13.5","...3..36..",".676.4....",".3..77..31",".13.8...1.","......3..."]

Można założyć poprawność wczytywanego pliku. Nie ma ograniczeń na wielkość planszy. Po zna-lezieniu rozwiązania, aplikacja powinna je wyświetlić w formie tekstowej na ekranie.


## Uruchomienie aplikacji:
Należy znaleźć się w folderze `mosaic`

Zbudowanie:

 `stack build`

Uruchomienie:

 `stack run`