-- Projekt nr 9 - Kryptytmy - Dominik Marczuk

-- Wczytujemy z pliku dzialania dodawania i mnozenia w postaci liter
-- i wypisujemy wszystkie mozliwe rozwiazania (podstawienia cyfry za liczby, ktore daja poprawne dzialanie)
-- 1-sza wersja rozwiazania(zakomentowana) jest dla zalozenia ze jedna cyfra moze byc  
-- przyporzadkowana do kilku liter (tzn np A=1, B=1)
-- 2-ga wersja zaklada ze kazda litera musi miec inna cyfre

-- Przykladowy plik wejsciowy (file.txt)

--A+B=AC
--A+B+C=AC
--KTO+KOT=TOK
--POL+POL=CALA
--U*ULI=LIS
--AB*B*C=DA

--Wyjscie

--A+B=AC
--1+9=10

--A+B+C=AC
--1+9+0=10
--1+9+2=12
--1+9+3=13
--1+9+4=14
--1+9+5=15
--1+9+6=16
--1+9+7=17
--1+9+8=18

--KTO+KOT=TOK
--495+459=954

--POL+POL=CALA
--237+237=0474
--924+924=1848

--U*ULI=LIS
--2*249=498

--AB*B*C=DA
--04*4*5=80
--42*2*1=84



import System.IO;

-- Wypisanie kryptarytmu i rozwiazan, wywolujemy algorytm dla dzialania(kryptarytmu), 
-- listy wszystkich liter oraz listy dostepnych cyfr
work :: String -> String
work line = line ++ "\n" ++ solve line (arrayOfLetters line) ['0','1','2','3','4','5','6','7','8','9']
                             ++ "\n"

-- Wersja 1 - algorytm sprawdza wszystkie mozliwe podstawienia cyfr dla liter wystepujacych 
-- w dzialaniu (1 cyfra moze byc dla wielu liter)
--solve :: String -> String -> String
--solve line [] =  if (checkOperation line == True) then line ++ "\n"  else []
--solve line (x:leterssToAssign) = solve (change line x '0') leterssToAssign ++
--                                 solve (change line x '1') leterssToAssign ++
--                                 solve (change line x '2') leterssToAssign ++
--                                 solve (change line x '3') leterssToAssign ++
--                                 solve (change line x '4') leterssToAssign ++
--                                 solve (change line x '5') leterssToAssign ++
--                                 solve (change line x '6') leterssToAssign ++
--                                 solve (change line x '7') leterssToAssign ++
--                                 solve (change line x '8') leterssToAssign ++
--                                 solve (change line x '9') leterssToAssign


-- Wersja 2, czyli 1 cyfra przypisana max do 1 litery
-- Jesli lista liter jest pusta do sprawdzamy czy dzialanie jest prawidlowe
-- Jesli nadal mamy litery do podmienienia to bierzemy kolejna litere i wywolujemy
-- funkcje sprawdzajaca wszystkie mozliwe podstawienia cyfr z listy dostepnych cyfr 
solve :: String -> String -> [Char] -> String
solve line [] list =  if (checkOperation line == True) then line ++ "\n"  else [] --line ++ "\n" 
solve line (x:leterssToAssign) list = rek2 line x leterssToAssign (createList line ['0','1','2','3','4','5','6','7','8','9'])

-- Jesli lista cyfr jest pusta - sprawdzilismy wszystkie mozliowsci i wracamy
-- Jesli mamy dostepne cyfry to "bierzemy" kolejna cyfre i wywolujemy funkcje solve(z dzialaniem z podmieniona cyfra za litere)
-- az wszystkie litery beda podmienione i wtedy wywolujemy funkcje rek2 dla pozostalych cyfr
rek2 :: String -> Char -> String -> [Char] -> String
rek2 line x leterssToAssign []  = []
rek2 line x leterssToAssign (xs:list) = solve (change line x xs) leterssToAssign list ++ rek2 line x leterssToAssign list

-- funckja tworzaca liste cyfr jeszcze nie uzytych w dzialaniu
createList :: String -> [Char] -> [Char]
createList line [] = []
createList line (x:list) = if (isIn x line) then createList line list else [x]++createList line list

isIn :: Char -> String -> Bool
isIn x [] = False
isIn x (xs:line) = if (x == xs) then True else isIn x line 

--sprawdzamy poprawnosc dzialania zapisanego jako String
checkOperation :: String -> Bool
checkOperation line = (parser line line) == parserToInt(reverse(parser3 line))

-- Funkcja ktora podmienia litere wystepujaca w dzialaniu na dana cyfre
change :: String -> Char -> Char -> String
change [] letter num = ""
change (x:line) letter num = if (x == letter) then [num] ++ change line letter num
                               else [x] ++ change line letter num

-- Obliczamy wartosc liczbowa dzialania zapisanego jako String
-- liczymy "od tylu" tzn dla odwroconego napisu (liczby)
parserToInt :: String -> Int
parserToInt [] = 0
parserToInt (x:line) = (digitToInt x) + (10 * (parserToInt line))

digitToInt :: Char -> Int
digitToInt a | (a == '0') = 0
             |( a == '1') = 1
             |( a == '2') = 2
             |( a == '3') = 3
             |( a == '4') = 4
             |( a == '5') = 5
             |( a == '6') = 6
             |( a == '7') = 7
             |( a == '8') = 8
             |( a == '9') = 9
             | otherwise = 0 -- -1 

-- parsujemy lewa czesc rownania, tzn funkcja zwraca wartosc liczbowa lewej strony dzialania
-- obliczamy wartosc 1-szego skladnika i rekurencyjnie wywolujemy funkcje az dojdziemy do znaku "="
parser :: String -> String -> Int
parser [] line2 = 0
parser (x:line) line2   | x == '+' = (parserToInt(reverse(parser1 line2))) + (parser line line)
                        | x == '*' = (parserToInt(reverse(parser1 line2))) * (parser line line)
                        | x == '=' = (parserToInt(reverse(parser1 line2)))
                        | otherwise = parser line line2

-- parsujemy skladnik dzialania (litery az napotkamy znak +,* lub =)
-- czyli funkcja zwraca pojednyczny skladnik po lewej stronie rownania
parser1 :: String -> String
parser1 [] = ""
parser1 (x:line) | x == '+' = ""
                 | x == '*' = ""
                 | x == '=' = "" 
                 | otherwise = [x] ++ parser1 line

-- analogicznie jak parser1 tylko bierzemy skladnik po prawej stronie
parser3 :: String -> String
parser3 [] = ""
parser3 (x:line) = if (x == '=') then parser1 line 
                     else parser3 line

--tworzenie listy liter wystepujacych w dzialaniu
arrayOfLetters :: String -> String
arrayOfLetters [] = ""
arrayOfLetters (x:xs) = [x] ++ arrayOfLetters (deldup x xs)

--usuwanie duplikatow
deldup :: Char -> String -> String
deldup a [] = ""
deldup a (x:line) | (a == x) = deldup a line 
                  | (x == '=') = deldup a line
                  | (x == '+') = deldup a line
                  | (x == '*') = deldup a line
                  | otherwise = [x] ++ (deldup a line)

--dla kazdej linii wywolujemy funkcje work(dzialanie algorytmu)
showFile :: Handle -> IO ()
showFile handle = do
              eof<-hIsEOF handle
              if eof then return () 
                else 
                  do
                    line<-hGetLine handle
                    putStrLn (work line)
                    showFile handle

--wczytujemy plik
main = do
           fileHandle <-openFile "file.txt" ReadMode
           showFile fileHandle
           hClose fileHandle
