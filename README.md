# Projekt Kryptarytmy

## Opis projektu

Wczytujemy z pliku dzialania dodawania i mnozenia w postaci liter
i wypisujemy wszystkie mozliwe rozwiazania (podstawienia cyfry za liczby, ktore daja poprawne dzialanie)
1-sza wersja rozwiazania(zakomentowana) jest dla zalozenia ze jedna cyfra moze byc  
przyporzadkowana do kilku liter (tzn np A=1, B=1)
2-ga wersja zaklada ze kazda litera musi miec inna cyfre

## Przykladowy plik wejsciowy (file.txt)

A+B=AC__
A+B+C=AC__
KTO+KOT=TOK__
POL+POL=CALA__
U*ULI=LIS__
AB*B*C=DA__

Wyjscie

A+B=AC__
1+9=10__

A+B+C=AC__
1+9+0=10__
1+9+2=12__
1+9+3=13__
1+9+4=14__
1+9+5=15__
1+9+6=16__
1+9+7=17__
1+9+8=18__

KTO+KOT=TOK__
495+459=954__

POL+POL=CALA__
237+237=0474__
924+924=1848__

U*ULI=LIS__
2*249=498__

AB*B*C=DA__
04*4*5=80__
42*2*1=84__
