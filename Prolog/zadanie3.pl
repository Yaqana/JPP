% Michal Moskal
% JPP - zadanie 3


% Stan w programie reprezentowany jest jako para (Env, Liczniki).
% Srodowisko Env sklada sie zarowno ze zmiennych zwyklych jak i tablicowych.
% Zmienne zwykle reprezentowane sa jako para (id, wartosc), a tablicowe
% jako para (id, wartosci), gdzie wartosci to pary (id, wartosc).
% Liczniki to lista N wartosci odpowiadajacyh licznikom rozkazow procesow 0...N-1

:- ensure_loaded(library(lists)).

% zmienna(Env, Identyfikator, PrId, Wynik)
% Oblicza wartosci zmiennych
zmienna(_, pid, PrId, Wynik) :- Wynik is PrId, !.

zmienna(Env, Ident, _, Wynik) :- atom(Ident), member((Ident, Wynik), Env), !.

zmienna(Env, arr(Ident, WyrArytm), PrId, Wynik) :-
    atom(Ident),
    wyrArytm(WyrArytm, Env, PrId, Indeks),
    member((Ident, Tablica), Env),
    member((Indeks, Wynik), Tablica).

% Wyrazenia arytmetyczne
wyrProste(Liczba, _Env, _PrId, Wynik) :-
    integer(Liczba), Wynik is Liczba,
    !.

wyrProste(Zmienna, Env, PrId, Wynik)  :-
    zmienna(Env, Zmienna, PrId, Liczba),
    Wynik is Liczba,
    !.

wyrArytm(WyrProste, Env, PrId, Wynik) :-
    wyrProste(WyrProste, Env, PrId, Wynik),
    !.

wyrArytm(+(WyrProste1, WyrProste2), Env, PrId, Wynik) :-
    wyrProste(WyrProste1, Env, PrId, W1),
    wyrProste(WyrProste2, Env, PrId, W2),
    Wynik is W1 + W2,
    !.

wyrArytm(-(WyrProste1, WyrProste2), Env, PrId, Wynik) :-
    wyrProste(WyrProste1, Env, PrId, W1),
    wyrProste(WyrProste2, Env, PrId, W2),
    Wynik is W1 - W2,
    !.

wyrArytm(*(WyrProste1, WyrProste2), Env, PrId, Wynik) :-
    wyrProste(WyrProste1, Env, PrId, W1),
    wyrProste(WyrProste2, Env, PrId, W2),
    Wynik is W1 * W2,
    !.

wyrArytm(/(WyrProste1, WyrProste2), Env, PrId, Wynik) :-
    wyrProste(WyrProste1, Env, PrId, W1),
    wyrProste(WyrProste2, Env, PrId, W2),
    Wynik is div(W1, W2),
    !.


% Wyrazenia logiczne
wyrLogiczne(<(WyrProste1, WyrProste2), Env, PrId, Wynik) :-
    wyrProste(WyrProste1, Env, PrId, W1),
    wyrProste(WyrProste2, Env, PrId, W2),
    W1 < W2,
    Wynik is 1,
    !.

wyrLogiczne(<(WyrProste1, WyrProste2), Env, PrId, Wynik) :-
    wyrProste(WyrProste1, Env, PrId, W1),
    wyrProste(WyrProste2, Env, PrId, W2),
    W1 >= W2,
    Wynik is 0.

wyrLogiczne(=(WyrProste1, WyrProste2), Env, PrId, Wynik) :-
    wyrProste(WyrProste1, Env, PrId, W1),
    wyrProste(WyrProste2, Env, PrId, W2),
    W1 =:= W2,
    Wynik is 1.

wyrLogiczne(=(WyrProste1, WyrProste2), Env, PrId, Wynik) :-
    wyrProste(WyrProste1, Env, PrId, W1),
    wyrProste(WyrProste2, Env, PrId, W2),
    W1 =\= W2,
    Wynik is 0,
    !.

wyrLogiczne(<>(WyrProste1, WyrProste2), Env, PrId, Wynik) :-
    wyrProste(WyrProste1, Env, PrId, W1),
    wyrProste(WyrProste2, Env, PrId, W2),
    W1 =\= W2,
    Wynik is 1,
    !.

wyrLogiczne(<>(WyrProste1, WyrProste2), Env, PrId, Wynik) :-
    wyrProste(WyrProste1, Env, PrId, W1),
    wyrProste(WyrProste2, Env, PrId, W2),
    W1 =:= W2,
    Wynik is 0,
    !.

:- op(700, xfx, <>).
<>(W1, W2) :- W1 =\= W2.


% zamien(Tablica, Indeks, Liczba, TablicaWynik)
% Zamienia w tablicy wartosc pod indeksem Indeks na Liczba
zamien([(I, _Wartosc)|Tablica], I, Liczba, [(I, Liczba)|Tablica]) :- !.

zamien([(I2, Wartosc)|Tablica1], I1, Liczba, [(I2, Wartosc)|Tablica2]) :-
    dif(I2, I1),
    zamien(Tablica1, I1, Liczba, Tablica2).

% przypiszZmienna(EnvWejscie, Ident, Liczba, EnvWyjscie)
% Nadpisuje wartosc zmiennej Ident w srodowisku na Liczba
przypiszZmienna([(Id, _)|EnvWe], Id, Liczba, [(Id, Liczba)|EnvWe]) :- !.

przypiszZmienna([(Id2, Wartosc)|EnvWe], Id1, Liczba, [(Id2, Wartosc)|EnvWy]) :-
    dif(Id2, Id1),
    przypiszZmienna(EnvWe, Id1, Liczba, EnvWy).


% przypiszTablica(EnvWe, Zmienna tablicowa, Liczba, EnvWy)
% Nadpisuje wartosc w podanej tablicy i indeksie na Liczba 
przypiszTablica([(Id, Tab)|EnvWe], arr(Id, Indeks), L, [(Id, Tab2)|EnvWe]) :-
    integer(Indeks),
    zamien(Tab, Indeks, L, Tab2),
    !.

przypiszTablica([(Id, Tab)|EnvWe], arr(Ident, Indeks), L, [(Id, Tab)|EnvWy]) :-
    integer(Indeks),
    dif(Id, Ident),
    przypiszTablica(EnvWe, arr(Ident, Indeks), L, EnvWy).


% przypisz(EnvWe, PrId, Zmienna, Wynik, EnvWy)
% Uogolnienie przypiszZmienna i przypiszTablica
przypisz(_, _, pid, _, _) :- fail.

przypisz(EnvWe, _, Ident, Liczba, EnvWy) :-
    atom(Ident),
    przypiszZmienna(EnvWe, Ident, Liczba, EnvWy).


przypisz(EnvWe, PrId, arr(Ident, WyrArytm), Liczba, EnvWy) :-
    wyrArytm(WyrArytm, EnvWe, PrId, Indeks),
    przypiszTablica(EnvWe, arr(Ident, Indeks), Liczba, EnvWy).


% aktualizuj(LiczWe, PrId, Numer, LiczWy)
% Zmienia wartosc licznika rozkazow procesu PrId na Numer
aktualizuj([_|LiczWe], 0, Numer, [Numer|LiczWe]) :- !.
    
aktualizuj([L|LiczWe], PrId, Numer, [L|LiczWy]) :-
    integer(PrId),
    PrId > 0,
    NoweId is PrId - 1,
    aktualizuj(LiczWe, NoweId, Numer, LiczWy).    

% nastepna(LiczWe, PrId, LiczWy)
% Zwieksza wartosc licznika rozkazow procesu PrId o 1
nastepna([L|LiczWe], 0, [Nastepna|LiczWe]) :-
    Nastepna is L + 1, % zakladamy, ze mozna zrobic +1
    !.

nastepna([L|LiczWe], PrId, [L|LiczWy]) :-
    integer(PrId),
    PrId > 0,
    NoweId is PrId - 1,
    nastepna(LiczWe, NoweId, LiczWy).



% assign(Zmienna, WyrArytm, StanWe, PrId, StanWy)			 
assign(Zmienna, WyrArytm, (EnvWe, LiczWe), PrId, (EnvWy, LiczWy)) :-
    wyrArytm(WyrArytm, EnvWe, PrId, Wynik),
    przypisz(EnvWe, PrId, Zmienna, Wynik, EnvWy),
    nastepna(LiczWe, PrId, LiczWy).

% goto(Liczba, StanWe, PrId, StanWy)
goto(Liczba, (EnvWe, LiczWe), PrId, (EnvWe, LiczWy)) :-
    integer(Liczba),
    aktualizuj(LiczWe, PrId, Liczba, LiczWy).

% condGoto(WyrLogiczne, Instrukcja,  StanWe, PrId, StanWy)
condGoto(WyrLogiczne, Instrukcja, (EnvWe, LiczWe), PrId, (EnvWe, LiczWy)) :-
    wyrLogiczne(WyrLogiczne, EnvWe, PrId, Wynik),
    (   Wynik is 1
     -> aktualizuj(LiczWe, PrId, Instrukcja, LiczWy)
     ;  nastepna(LiczWe, PrId, LiczWy)
    ).

sekcja((Env, LiczWe), PrId, (Env, LiczWy)) :-
    nastepna(LiczWe, PrId, LiczWy).

% Zwraca wartosc licznika procesu PrId 
aktualny(Liczniki, PrId, Licznik) :- nth0(PrId, Liczniki, Licznik).

% Zwraca numer instrukcji, ktorej znajduje sie proces z licznikiem
% o wartosci Licznik
instrukcja(ListaInstrukcji, Licznik, Instrukcja) :-
    nth1(Licznik, ListaInstrukcji, Instrukcja).


% step(ListaInstrukcji, StanWe, PrId, StanWy)
step([], _, _, _).

step(ListaInstrukcji, (EnvWe, LiczWe), PrId, StanWy) :-
    aktualny(LiczWe, PrId, Licznik),
    instrukcja(ListaInstrukcji, Licznik, Instrukcja),
    call(Instrukcja, (EnvWe, LiczWe), PrId, StanWy).


% Zwraca liste zmiennych zainicjalizowanych na 0
inicjujZmienne([], []).

inicjujZmienne([Z|Zmienne], [(Z, 0)|Env]) :-
    inicjujZmienne(Zmienne, Env).
    
% Zwraca liste tablic zainicjalizowanych na 0
inicjujTablice([], _, []) :- !.

inicjujTablice([T|Tablice], Zera, [(T, Zera)|Env]) :-
    inicjujTablice(Tablice, Zera, Env).

% Zwraca tablice N zer
tablicaZera(0, []) :- !.

tablicaZera(N, [(M, 0)|Zera]) :-
    M is N - 1,
    tablicaZera(M, Zera).


% Zwraca liste licznikow zainicjalizowanych na 1
licznikiJeden(0, []) :- !.

licznikiJeden(N, [1|Jedynki]) :-
    M is N - 1,
    licznikiJeden(M, Jedynki).

% initStte(Program, M, Stan)
% Zwraca zainincjalizowany stan poczatkowy.
initState(ZmienneProste, ZmienneTablicowe, N, (Srodowisko, Jedynki)) :-
    inicjujZmienne(ZmienneProste, SrodowiskoZmienne),
    tablicaZera(N, Zera),
    inicjujTablice(ZmienneTablicowe, Zera, SrodowiskoTablice),
    append(SrodowiskoZmienne, SrodowiskoTablice, Srodowisko),
    licznikiJeden(N, Jedynki).

% niebezpiecznyStan(ListaInstrukcji, Stan, Proces1, Proces2)
% Sprawdza, czy dany stan jest bezpieczny. Jesli nie, zmienne
% Proces1 i Proces2 unifikuja sie do par (P, L), gdzie P
% oznacza numer procesu, a L numer instrukcji.
niebezpiecznyStan(ListaInstrukcji, (_Env, Liczniki), (P1, L1), (P2, L2)) :-
    nth0(P1, Liczniki, L1),
    nth0(P2, Liczniki, L2),
    P1 =\= P2,
    nth1(L1, ListaInstrukcji, sekcja),
    nth1(L2, ListaInstrukcji, sekcja).

% push(ListaInstrukcji, PrId, StanWe, Odwiedzone, StosWe, StosWy)
% Wykonuje jeden krok programu dla procesu PrId. Jesli stan koncowy nie
% byl do tej pory odwiedzony, to zostaje dodany do stosu wierzcholkow
% oczekujacych na przetworzenie
push(ListaInstrukcji, 0, StanWe, Odwiedzone, Stos, [Stan|Stos]) :-
    step(ListaInstrukcji, StanWe, 0, Stan),
    \+ member(Stan, Odwiedzone),
    !.

push(ListaInstrukcji, 0, StanWe, Odwiedzone, Stos, Stos) :-
    step(ListaInstrukcji, StanWe, 0, Stan),
    member(Stan, Odwiedzone),
    !.

push(ListaInstrukcji, N, StanWe, Odwiedzone, Stos, NowyStos) :-
    N > 0,
    M is N - 1,
    step(ListaInstrukcji, StanWe, N, Stan),
    member(Stan, Odwiedzone),
    !,
    push(ListaInstrukcji, M, StanWe, Odwiedzone, Stos, NowyStos).


push(ListaInstrukcji, N, StanPoczatkowy, Odwiedzone, Stos, [Stan|NowyStos]) :-
    N > 0,
    M is N - 1,
    step(ListaInstrukcji, StanPoczatkowy, N, Stan),
    \+ member(Stan, Odwiedzone),
    push(ListaInstrukcji, M, StanPoczatkowy, Odwiedzone, Stos, NowyStos).

% niebezpiecznyPrzeplot(ListaInstrukcji, N, Odwiedzone, Stos, Numer)
% Przeszukuje przestrzen stanow w poszukiwaniu niebezpiecznego stanu.
% Kolejne stany oczekujace na przetworzenie zostaja dodane do stosu
niebezpiecznyPrzeplot(ListaInstrukcji, _N, _Odwiedzone, [Stan|_Stos], Numer) :-
    niebezpiecznyStan(ListaInstrukcji, Stan, (P1, L1), (P2, L2)),
    !,
    write('Program jest niepoprawny: stan nr '),
    write(Numer),
    write(' nie jest bezpieczny.'), nl,
    write('Niepoprawny przeplot:'), nl,
    write('  Proces '), write(P1), write(': '), write(L1), nl,
    write('  Proces '), write(P2), write(': '), write(L2), nl,
    write('Procesy w sekcji: '), write(P1), write(', '), write(P2), write('.').
	
niebezpiecznyPrzeplot(ListaInstr, N, Odwiedzone, [Stan|Stos], Numer) :-
    M is N - 1,
    push(ListaInstr, M, Stan, Odwiedzone, Stos, NowyStos),
    NowyNumer is Numer + 1,
    niebezpiecznyPrzeplot(ListaInstr, N, [Stan|Odwiedzone], NowyStos, NowyNumer).

przeszukaj(ListaInstrukcji, N, StanPoczatkowy) :-
    niebezpiecznyPrzeplot(ListaInstrukcji, N, [], [StanPoczatkowy|[]], 1),
    !.

przeszukaj(_, _, _) :-
    write('Program jest poprawny (bezpieczny).').

verify(N, _) :-
    N < 1,
    !,
    write('Error: parametr N powinien byc liczba > 0.'),
    fail.

verify(N, Program) :-
    set_prolog_flag(fileerrors, off),
    see(Program),
    !,
    read(vars(Zmienne)),
    read(arrays(Tablice)),
    read(program(Instrukcje)),
    seen,
    
    initState(Zmienne, Tablice, N, StanPoczatkowy),
    przeszukaj(Instrukcje, N, StanPoczatkowy).

verify(_, Program) :-
    format('Error: brak pliku o nazwie - ~p.~n', [Program]),
    fail.
