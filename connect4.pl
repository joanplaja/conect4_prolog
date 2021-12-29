% This is how board looks like.
% Where x and o represents discs of diferents players and - empty spaces
% board([
%     [x,-,-,-,-,-,-],
%     [x,-,-,-,-,-,-],
%     [x,-,-,-,-,-,-],
%     [o,-,-,-,-,-,-],
%     [o,-,-,-,-,-,-],
%     [o,-,-,-,-,-,-]
% ]).
% board([[x,-,-,-,-,-,-],[x,-,-,-,-,-,-],[x,-,-,-,-,-,-],[o,-,-,-,-,-,-],[o,-,-,-,-,-,-],[o,-,-,-,-,-,-]])

% Import library
:- [library].

% checkPlayerCollection(Collections,Player)
% The Player has four followed discs in one Collection.
% Collections represents the list of rows,columns or diagonals.
checkPlayerCollection([Collection|Rest],human) :-
    append(_,[x,x,x,x|_],Collection),!.
checkPlayerCollection([Collection|Rest],human) :-
    checkPlayerCollection(Rest,human).
checkPlayerCollection([Collection|Rest],cpu) :-
    append(_,[o,o,o,o|_],Collection),!.
checkPlayerCollection([Collection|Rest],cpu) :-
    checkPlayerCollection(Rest,cpu).

% primeraColumna(M,C,S)es satisfa si C es la primera columna de la matriuMi si S  ́es la matriu M sense la primera columna C.
primeraColumna([],[],[]). %matriu buida
primeraColumna([[Primer|PrimeraFila]|RestaFiles],[Primer|RestaColumna],[PrimeraFila|Resta]) :-
    primeraColumna(RestaFiles,RestaColumna,Resta).
% test: primeraColumna([[x,-,-,-,-,-,-],[x,-,-,-,-,-,-],[x,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-]],Primera,Resta).

%transposicio(M,T) T és la matriu transposta de M
transposicio([[]|_],[]) :- !. %si primera fila no queden elements
transposicio([[X|Fila]|Files],[Columna|AltresColumnes]) :-
    primeraColumna([[X|Fila]|Files],Columna,[Fila|Resta]), %obtenim la primera columna i la afegim al conjunt
    transposicio([Fila|Resta],AltresColumnes). %obtenim la resta de columnes
% test: transposicio([[x,-,-,-,-,-,-],[x,-,-,-,-,-,-],[x,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-]],T).

% horitzontal(tauler(X),X)} Es satisfa si X és la mateixa matriu que la que hi ha al tauler.
horitzontal(tauler(X),X).
% test: horitzontal(tauler([[x,-,-,-,-,-,-],[x,-,-,-,-,-,-],[x,-,-,-,-,-,-],[o,-,-,-,-,-,-],[o,-,-,-,-,-,-],[o,-,-,-,-,-,-]]),H).

%vertical(tauler(X),C) es satisfa si C es la matriu transposta de la matriu X.
vertical(tauler(X),C) :- transposicio(X,C).
% test: vertical(tauler([[x,-,-,-,-,-,-],[x,-,-,-,-,-,-],[x,-,-,-,-,-,-],[o,-,-,-,-,-,-],[o,-,-,-,-,-,-],[o,-,-,-,-,-,-]]),V).


% TOT ELS SEGUENTS SERVEIXENT PER CONSTRUIR EL DE LES DIAGONALS

%obtenirPosicio(C,P,E) E és l element del conjunt C a la posicio P
obtenirPosicio([X|XS],0,X) :- !.
obtenirPosicio([X|XS],N,R) :- Np is N-1, obtenirPosicio(XS,Np,R).
% test = obtenirPosicio([x,-,o],2,X).

% invers(C,Cp) es satisfa si Cp és el Conjunt C invertit
invers([],[]).
invers([X|Xs],In):-invers(Xs,Ps), append(Ps,[X],In).

%reversePosicionsFiles(M,Mp) Es satisfa si Mp es equivalent a la matriu M peró amb les files invertides. És utilitzat per obtenir les diagonals negatives.
reversePosicionsFiles([],[]).
reversePosicionsFiles([X|XS],[Y|YS]) :- invers(X,Y), reversePosicionsFiles(XS,YS).
%test = reversePosicionsFiles([[x,o,-,x,o,-,x],[o,-,x,o,-,x,o],[-,x,o,-,x,o,-],[x,o,-,x,o,-,x],[o,-,x,o,-,x,o],[-,x,o,-,x,o,-]],T).

eq(7,6).
eq(8,5).
eq(9,4).
eq(10,3).
eq(11,2).
eq(12,1).
eq(13,0).

% obtenirDiagonalPositiva(M,N,D) es satisfa si D és la diagonal postiva número N de la matriu M.
obtenirDiagonalPositiva(M,N,R) :-
    N < 7, iobtenirDiagonalPositiva(M,N,R).
obtenirDiagonalPositiva(M,N,R) :-
    N > 6,
    eq(N,Np),
    reversePosicionsFiles(M,Mp),invers(Mp,Mpp),
    iobtenirDiagonalPositiva(Mpp,Np,R).
%test = obtenirDiagonalPositiva([[x,o,-,x,o,-,x],[o,-,x,o,-,x,o],[-,x,o,-,x,o,-],[x,o,-,x,o,-,x],[o,-,x,o,-,x,o],[-,x,o,-,x,o,-]],1,D).


%iobtenirDiagonalPositiva(M,N,D) predicat immersiu, idea és començar fila 1 matriu a la posicio N ( numero de diagonal ) i obtenir l'element, llavors fer el mateix per la seguent
%fila pero amb N-1 fins que N=0 ja que ja no quedaran files i tindrem tota la diagonal.
iobtenirDiagonalPositiva(L,0,[]) :- !.
iobtenirDiagonalPositiva([Fila|Files],N,[X|R]) :-
    Np is N-1, %fem abans pq obtenir posicio utilitza 0 per saber primer element
    obtenirPosicio(Fila,Np,X), % per aixo utiltizem Np
    iobtenirDiagonalPositiva(Files,Np,R).
%test = iobtenirDiagonalPositiva([[x,o,-,x,o,-,x],[o,-,x,o,-,x,o],[-,x,o,-,x,o,-],[x,o,-,x,o,-,x],[o,-,x,o,-,x,o],[-,x,o,-,x,o,-]],6,D).


% obtenirDiagonalNegativa(M,N,D)} es satisfa si D és la diagonal negativa número N de la matriu M.
obtenirDiagonalNegativa(M,N,R) :-
    invers(M,Mp), %si fem inversa es el mateix que obtenir la positiva
    obtenirDiagonalPositiva(Mp,N,R).
%obtenirDiagonalNegativa([[x,o,-,x,o,-,x],[o,-,x,o,-,x,o],[-,x,o,-,x,o,-],[x,o,-,x,o,-,x],[o,-,x,o,-,x,o],[-,x,o,-,x,o,-]],9,D).


% diagonals(tauler(X),C)} C és el conjunt de conjunts de diagonals positives i negatives de la matriu X del tauler, on cada conjunt té minim una longitud de 4, ja que sino no és possible fer 4 en ralla i no ens interessa.
% DIAGONALS POSITIVES I NEGATIVES UTILS SON QUE PODEN SER 4 PER TANT, 4,5,6,7,8,9
diagonals(tauler(X),[P4,P5,P6,P7,P8,P9,N4,N5,N6,N7,N8,N9]) :-
    obtenirDiagonalPositiva(X,4,P4),
    obtenirDiagonalPositiva(X,5,P5),
    obtenirDiagonalPositiva(X,6,P6),
    obtenirDiagonalPositiva(X,7,P7),
    obtenirDiagonalPositiva(X,8,P8),
    obtenirDiagonalPositiva(X,9,P9),
    obtenirDiagonalNegativa(X,4,N4),
    obtenirDiagonalNegativa(X,5,N5),
    obtenirDiagonalNegativa(X,6,N6),
    obtenirDiagonalNegativa(X,7,N7),
    obtenirDiagonalNegativa(X,8,N8),
    obtenirDiagonalNegativa(X,9,N9).
%test = diagonals(tauler([[x,o,-,x,o,-,x],[o,-,x,o,-,x,o],[-,x,o,-,x,o,-],[x,o,-,x,o,-,x],[o,-,x,o,-,x,o],[-,x,o,-,x,o,-]]),D).
%test: diagonals(tauler([[x,-,-,-,-,-,-],[x,-,-,-,-,-,-],[x,-,-,-,-,-,-],[o,-,-,-,-,-,-],[o,-,-,-,-,-,-],[o,-,-,-,-,-,-]]),V).


%isWinner(tauler(X),Jugador) es satisfa si el jugador Jugador té 4 en ralla en el tauler.
isWinner(tauler(X),Jugador) :-
    horitzontal(tauler(X),H),
    checkPlayerCollection(H,Jugador),!.
isWinner(tauler(X),Jugador) :-
    vertical(tauler(X),V),
    checkPlayerCollection(V,Jugador),!.
isWinner(tauler(X),Jugador) :-
    diagonals(tauler(X),D),
    checkPlayerCollection(D,Jugador),!.

% tauler([
%     [-,-,-,-,-,-,-],
%     [-,-,-,-,-,-,-],
%     [-,-,-,-,-,-,-],
%     [-,-,-,-,-,-,-],
%     [-,-,-,-,-,-,-],
%     [x,x,x,x,o,o,o]
% ]).
% test guanya horitzontal huma = isWinner(tauler([[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[x,x,x,x,o,o,o]]),huma).
% test guanya horitzontal cpu = isWinner(tauler([[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[x,x,x,o,o,o,o]]),cpu).
% testos inversos detecta no guanya:
%isWinner(tauler([[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[x,x,x,x,o,o,o]]),cpu).
%isWinner(tauler([[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[x,x,x,o,o,o,o]]),huma).
% tauler([
%     [-,-,-,-,-,-,-],
%     [-,-,-,-,-,-,-],
%     [-,-,-,-,x,-,-],
%     [-,-,-,x,o,-,-],
%     [-,-,x,o,x,-,-],
%     [x,x,o,x,o,o,o]
% ]).
%test guanya huma diagonal = isWinner(tauler([[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,x,-,-],[-,-,-,x,o,-,-],[-,-,x,o,x,-,-],[x,x,o,x,o,o,o]]),huma).
% tauler([
%     [-,-,-,-,-,-,-],
%     [-,-,-,-,-,-,-],
%     [-,-,x,-,o,-,-],
%     [-,-,x,x,o,-,-],
%     [-,-,x,o,x,-,-],
%     [x,x,o,x,o,x,o]
% ]).
% diagonal negativa isWinner(tauler([[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,x,-,o,-,-],[-,-,x,x,o,-,-],[-,-,x,o,x,-,-],[x,x,o,x,o,x,o]]),huma).
% tauler([
%     [-,-,-,-,-,-,-],
%     [-,-,-,-,-,-,-],
%     [-,-,-,x,o,-,-],
%     [-,-,-,x,o,-,-],
%     [-,-,x,x,x,-,-],
%     [x,x,o,x,o,o,o]
% ]).
%test guanya huma vertical = isWinner(tauler([[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,x,o,-,-],[-,-,-,x,o,-,-],[-,-,x,x,x,-,-],[x,x,o,x,o,o,o]]),huma).
% vertical(tauler([[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,x,o,-,-],[-,-,-,x,o,-,-],[-,-,x,x,x,-,-],[x,x,o,x,o,o,o]]),V).

%print(F) mostra la fila F separada per espais
printFila([]).
printFila([X|XS]) :-
    format("~w ", [X]),
    printFila(XS).
%test = stringFila([-,-,-,-,-,-,-],X).

%mostrar(tauler(X)) es mostra el tauler amb el format especificat a la pràctica
mostrarTauler(tauler(X)) :-
    format("  A B C D E F G\n",[]),
    imostrarTauler(X,6).
%test mostrarTauler(tauler([[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,x,o,-,-],[-,-,-,x,o,-,-],[-,-,x,x,x,-,-],[x,x,o,x,o,o,o]])).
%test mostrarTauler(tauler([[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-]])).

%imostrarTauler(M,N) es mostra les N files de la matriu M
imostrarTauler([],N).
imostrarTauler([Fila|Files],N) :-
    format("~w ", [N]),
    printFila(Fila),
    format("~n",[]),
    Np is N-1,
    imostrarTauler(Files,Np).

repeat.
repeat :- repeat.

posicio(a,1).
posicio(b,2).
posicio(c,3).
posicio(d,4).
posicio(e,5).
posicio(f,6).
posicio(g,7).


%llegeix(X,Y) X és el valor llegit i X ha de ser una posicio existent i per tant si X és existent obtenim el corresponent valor númeric associat a la X, és a dir Y.
llegeix(X,Y):-
    repeat,
    format("Entra posicio [a-g] (recorda que la posicio sigui lliure i ficar un punt al final): ",[]),
    read(X),
    format("~n",[]),
    posicio(X,Y), !.

%validaPosicio(tauler(X),Y)es satisfa si Y és una posició valida per fer una tirada.
validaPosicio(tauler(X),Y) :- vertical(tauler(X),H), ivalidaPosicio(H,Y).

%ivalidaPosicio(tauler(X),N) és el predicat immersiu que comprova si la columna el primer element és buit o no
ivalidaPosicio([[-|Columna]|Columnes],1) :- !.
ivalidaPosicio([X|XS],N) :-
    Np is N-1,
    ivalidaPosicio(XS,Np).


% escollirJugada(tauler(X),Z) Z és un número entre 1 i 7 ( a - g ) i el número es una posicio valida per fer un moviment en el tauler.
escollirJugada(tauler(X),Z) :- repeat,llegeix(Y,Z),validaPosicio(tauler(X),Z), !.
% tauler([
%     [x,-,-,-,-,-,-],
%     [x,-,-,-,-,-,-],
%     [x,-,-,-,-,-,-],
%     [o,-,-,-,-,-,-],
%     [o,-,-,-,-,-,-],
%     [o,-,-,-,-,-,-]
% ]).
%
%test escollirJugada(tauler([[x,-,-,-,-,-,-],[x,-,-,-,-,-,-],[x,-,-,-,-,-,-],[o,-,-,-,-,-,-],[o,-,-,-,-,-,-],[o,-,-,-,-,-,-]]),Y).

%aplicarJugada(T,J,I,R) On R es el tauler resultant d'aplicar la jugada del jugador J a la posicio I al tauler T
aplicarJugada(tauler(X),huma,I,tauler(R)) :-
    vertical(tauler(X),V),
    iaplicarJugada(V,x,I,1,Rp),
    vertical(tauler(Rp),R). %tornem a deixar normal
aplicarJugada(tauler(X),cpu,I,tauler(R)) :-
    vertical(tauler(X),V),
    iaplicarJugada(V,o,I,1,Rp),
    vertical(tauler(Rp),R). %tornem a deixar normal
%test: aplicarJugada(tauler([[x,-,-,-,-,-,-],[x,-,-,-,-,-,-],[x,-,-,-,-,-,-],[o,-,-,-,-,-,-],[o,-,-,-,-,-,-],[o,-,-,-,-,-,-]]),huma,2,R).

%iaplicarJugada(M,F,P,N,R) R és la matriu resultant de posar la fitxa F a la posicio P quan P == N a la matriu M
iaplicarJugada([X|XS],Fitxa,I,I,[R|XS]) :-
    invers(X,Xinversa), %girar per ficar la fitxa abaix i no a dalt
    canviarPrimeraRalla(Xinversa,Fitxa,Rp),
    invers(Rp,R),
    !.
iaplicarJugada([X|XS],Fitxa,Pos,I,[X|R]) :-
    Ip is I+1,
    iaplicarJugada(XS,Fitxa,Pos,Ip,R).
% iaplicarJugada([[x,-,-,-,-,-,-],[x,-,-,-,-,-,-],[x,-,-,-,-,-,-],[o,-,-,-,-,-,-],[o,-,-,-,-,-,-],[o,-,-,-,-,-,-]],x,2,1,R).

%canviarPrimeraRalla()
canviarPrimeraRalla([-|XS],Fitxa,[Fitxa|XS]) :- !.
canviarPrimeraRalla([X|XS],Fitxa,[X|R]) :-
    canviarPrimeraRalla(XS,Fitxa,R).
%test = canviarPrimeraRalla([x,-,-,-,-,-,-],x,R).
%test = canviarPrimeraRalla([-,-,-,-,-,-,-],x,R).


startGameV1 :- rondaV1(tauler([[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-]]),huma).

%rondaV1(tauler(X),Jugador) predicat control de rondes
%si algun jugador ha guanyat mostrem que s'ha guanyat i acabem
rondaV1(tauler(X),_) :-
    isWinner(tauler(X),huma),
    format("!EL JUGADOR HUMA HA GUANYAT !", []),
    !.
rondaV1(tauler(X),_) :-
    isWinner(tauler(X),cpu),
    format("!EL JUGADOR CPU HA GUANYAT !", []),
    !.
%altramen si toca jugar huma, escolleix moviment
%apliquem moviment i cridem ronda amb jugador cpu
rondaV1(tauler(X),huma) :-
    mostrarTauler(tauler(X)),
    escollirJugada(tauler(X),Jugada),
    aplicarJugada(tauler(X),huma,Jugada,tauler(Resultant)),
    rondaV1(tauler(Resultant),cpu).
%el mateix per si es torn de cpu
rondaV1(tauler(X),cpu) :-
    mostrarTauler(tauler(X)),
    escollirJugada(tauler(X),Jugada),
    aplicarJugada(tauler(X),cpu,Jugada,tauler(Resultant)),
    rondaV1(tauler(Resultant),huma).

startGameV2 :- ronda(tauler([[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-]]),huma).

ronda(tauler(X),_) :-
    isWinner(tauler(X),huma),
    format("!EL JUGADOR HUMA HA GUANYAT !", []),
    !.
ronda(tauler(X),_) :-
    isWinner(tauler(X),cpu),
    format("!EL JUGADOR CPU HA GUANYAT !", []),
    !.
ronda(tauler(X),huma) :-
    %no es fi i no guanya contrincant
    mostrarTauler(tauler(X)),
    escollirJugada(tauler(X),Jugada),
    % write(Jugada),
    aplicarJugada(tauler(X),huma,Jugada,tauler(Resultant)),
    % mostrarTauler(tauler(Resultant)),
    ronda(tauler(Resultant),cpu).
ronda(tauler(X),cpu) :-
    %no es fi i no guanya contrincant
    %mostrarTauler(tauler(X)),
    % escollirJugada(tauler(X),Jugada),
    minmax(tauler(X),2,cpu,Valor,Jugada),
    aplicarJugada(tauler(X),cpu,Jugada,tauler(Resultant)),
    ronda(tauler(Resultant),huma).
% tauler([
%     [x,-,-,-,-,-,-],
%     [x,-,-,-,-,-,-],
%     [x,-,-,-,-,-,-],
%     [o,-,-,-,-,-,-],
%     [o,-,-,-,-,-,-],
%     [o,-,-,-,-,-,-]
% ]).
% ronda(tauler([[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-]]),huma).

%h(tauler(X),R) es satisfa si el guanyador del tauler es huma i R és -1 o el guanyador és CPU i R és 1 o sino ningu es guanyador i R és 0
h(tauler(X),-1) :- isWinner(tauler(X),huma),!.
h(tauler(X),1) :- isWinner(tauler(X),cpu),!.
h(tauler(X),0).

% movimentsPossibles(T,J,M) M conjunt de moviments possibles el tauler T
movimentsPossibles(tauler(X),Moviments) :-
    imovimentsPossibles(tauler(X),Moviments,1).
% test = movimentsPossibles(tauler([[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-]]),M).


imovimentsPossibles(tauler(X),[],8) :- !.
imovimentsPossibles(tauler(X),[I|MS],I) :-
    validaPosicio(tauler(X),I),
    Ip is I+1,
    imovimentsPossibles(tauler(X),MS,Ip),!.
imovimentsPossibles(tauler(X),MS,I) :-
    Ip is I+1,
    imovimentsPossibles(tauler(X),MS,Ip).

empty([]).

% minmax(tauler(X),PROF,J,P,M)
minmax(tauler(X),0,Jugador,P,M) :-
    M is 0, %moviment 0 no existeix == NULL
    h(tauler(X),P),!.
% si resultat es final retornem null perque no fa falta seguir explorant
minmax(tauler(X),Profunditat,Jugador,P,M) :-
    M is 0, %moviment 0 no existeix == NULL
    h(tauler(X),1),
    P is 1,
    !.
minmax(tauler(X),Profunditat,Jugador,P,M) :-
    M is 0, %moviment 0 no existeix == NULL
    h(tauler(X),-1),
    P is -1,
    !.
% sino hi ha cap moviment possible retornem null
minmax(tauler(X),Profunditat,Jugador,P,M) :-
    M is 0, %moviment 0 no existeix == NULL
    movimentsPossibles(tauler(X),Moviments),
    empty(Moviments),
    h(tauler(X),P),!.
%si juguen cpu maximitzem el resultat
minmax(tauler(X),Profunditat,cpu,PuntuacioEscollida,MovimentEscollit) :-
    %inicliatizem PuntuacioActual a -infinit amb un gran tenim suficient nomes sera 1 o -1
    PuntuacioActual is -10,
    % M is 0, %MovimentActual al principi es null 0 no existeix == NULL
    MovimentActual is 0,
    movimentsPossibles(tauler(X),Moviments),
    %profunditat(Profunditat),
    %format("minmax huma prof: ~w~n",[Profunditat]),
    buscarMillorMoviment(tauler(X),Profunditat,cpu,Moviments,PuntuacioActual,PuntuacioEscollida,MovimentActual,MovimentEscollit).
%si juguen huma minimitzem el resultat
minmax(tauler(X),Profunditat,huma,PuntuacioEscollida,MovimentEscollit) :-
    %profunditat(Profunditat),
    %format("minmax huma prof: ~w~n",[Profunditat]),
    %inicliatizem PuntuacioActual a -infinit amb un gran tenim suficient nomes sera 1 o -1
    PuntuacioActual is 10,
    % M is 0, %MovimentActual al principi es null 0 no existeix == NULL
    MovimentActual is 0,
    movimentsPossibles(tauler(X),Moviments),
    buscarPitjorMoviment(tauler(X),Profunditat,huma,Moviments,PuntuacioActual,PuntuacioEscollida,MovimentActual,MovimentEscollit).
% tauler([
%     [-,-,-,-,-,-,-],
%     [-,-,-,-,-,-,-],
%     [x,-,-,-,-,-,-],
%     [x,-,-,-,-,-,-],
%     [x,-,-,-,-,-,-],
%     [o,-,-,-,-,-,-]
% ]).
% tauler([[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[x,-,-,-,-,-,-],[x,-,-,-,-,-,-],[x,-,-,-,-,-,-],[o,-,-,-,-,-,-]])
% h(tauler([[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[x,-,-,-,-,-,-],[x,-,-,-,-,-,-],[x,-,-,-,-,-,-],[o,-,-,-,-,-,-]]),P).
% movimentsPossibles(tauler([[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[x,-,-,-,-,-,-],[x,-,-,-,-,-,-],[x,-,-,-,-,-,-],[o,-,-,-,-,-,-]]),M).
% minmax(tauler([[-,-,-,-,-,-,-],[x,-,-,-,-,-,-],[-,-,-,-,-,-,-],[x,-,-,-,-,-,-],[x,-,-,-,-,-,-],[o,-,-,-,-,-,-]]),1,cpu,Valor,Moviment).
% aquesta prova hauria de escollir 0 (primera columna) amb un valor de 0 si prof 1 ( primer nivell tots tornen 0)

% tauler([
%     [-,-,-,-,-,-,-],
%     [-,-,-,-,-,-,-],
%     [-,x,-,-,-,-,-],
%     [-,x,-,-,-,-,-],
%     [-,x,-,-,-,-,-],
%     [-,o,-,-,-,-,-]
% ]).
% aquesta prova hauria de escollir 2 (segona columna) amb un valor de 0 si prof 2 ( segon nivell si no escolleix 2 resultat = -1)
% minmax(tauler([[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,x,-,-,-,-,-],[-,x,-,-,-,-,-],[-,x,-,-,-,-,-],[-,o,-,-,-,-,-]]),2,cpu,Valor,Moviment).
%   simulacio nivell seguent minmax(tauler([[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,x,-,-,-,-,-],[-,x,-,-,-,-,-],[-,x,-,-,-,-,-],[-,o,-,-,-,-,-]]),1,huma,Valor,Moviment).
    % h(tauler([[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,x,-,-,-,-,-],[-,x,-,-,-,-,-],[-,x,-,-,-,-,-],[-,o,-,-,-,-,-]]),P).
    % movimentsPossibles(tauler([[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,x,-,-,-,-,-],[-,x,-,-,-,-,-],[-,x,-,-,-,-,-],[-,o,-,-,-,-,-]]),M).
    % buscarMillorMoviment(tauler([[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,x,-,-,-,-,-],[-,x,-,-,-,-,-],[-,x,-,-,-,-,-],[-,o,-,-,-,-,-]]),2,cpu,[1,2,3,4,5,6,7],-10,P,0,M).
        % minmax(tauler([[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,x,-,-,-,-,-],[-,x,-,-,-,-,-],[-,x,-,-,-,-,-],[o,o,-,-,-,-,-]]),1,huma,P,M).
        % h(tauler([[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,x,-,-,-,-,-],[-,x,-,-,-,-,-],[-,x,-,-,-,-,-],[o,o,-,-,-,-,-]]),P).
        % movimentsPossibles(tauler([[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,x,-,-,-,-,-],[-,x,-,-,-,-,-],[-,x,-,-,-,-,-],[o,o,-,-,-,-,-]]),M).
        % buscarPitjorMoviment(tauler([[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,x,-,-,-,-,-],[-,x,-,-,-,-,-],[-,x,-,-,-,-,-],[o,o,-,-,-,-,-]]),1,huma,[1,2,3,4,5,6,7],10,P,0,M).


%profunditat(N) s un predicat que escriu N guions
profunditat(0) :- !.
profunditat(N) :-
    format("-",[]),
    Np is N-1,
    profunditat(Np).

%sino hi ha mes jugades que analitzar
buscarMillorMoviment(tauler(X),Profunditat,Jugador,[],ValorEscollit,ValorEscollit,MovEscollit,MovEscollit).
%si juguen cpu maximitzem el resultat
buscarMillorMoviment(tauler(X),Profunditat,cpu,[Moviment|Moviments],ValorActual,ValorEscollit,MovActual,MovEscollit) :-
    aplicarJugada(tauler(X),cpu,Moviment,tauler(R)),
    Pp is Profunditat -1,
    minmax(tauler(R),Pp,huma,PuntuacioMINMAX,Mp),
    %profunditat(Profunditat),
    %format("bm moviment: ",[]),
    %format("~w  ", [Moviment]),
    %format("actual valor: ~w",[ValorActual]),
    %format("major valor: ",[]),
    %format("~w~n ", [PuntuacioMINMAX]),
    PuntuacioMINMAX > ValorActual,!,
    buscarMillorMoviment(tauler(X),Profunditat,cpu,Moviments,PuntuacioMINMAX,ValorEscollit,Moviment,MovEscollit).
buscarMillorMoviment(tauler(X),Profunditat,cpu,[Moviment|Moviments],ValorActual,ValorEscollit,MovActual,MovEscollit) :-
    aplicarJugada(tauler(X),cpu,Moviment,tauler(R)),
    Pp is Profunditat -1,
    minmax(tauler(R),Pp,huma,PuntuacioMINMAX,Mp),
    %profunditat(Profunditat),
    %format("bm xmoviment: ",[]),
    %format("~w  ", [Moviment]),
    %format("valor actual: ",[]),
    %format("~w~n ", [PuntuacioMINMAX]),
    PuntuacioMINMAX =< ValorActual,
    buscarMillorMoviment(tauler(X),Profunditat,cpu,Moviments,ValorActual,ValorEscollit,MovActual,MovEscollit).
% buscarMillorMoviment(tauler([[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,o,-,-,-,-,-],[-,o,-,-,-,-,-],[-,o,-,-,-,-,-],[-,x,-,-,-,-,-]]),1,cpu,[1,2,3,4,5,6,7],-10,P,M,MR).

%sino hi ha mes jugades que analitzar
buscarPitjorMoviment(tauler(X),Profunditat,Jugador,[],ValorEscollit,ValorEscollit,MovEscollit,MovEscollit) :- !.
%si juguen huma minimitzem el resultat
buscarPitjorMoviment(tauler(X),Profunditat,huma,[Moviment|Moviments],ValorActual,ValorEscollit,MovActual,MovEscollit) :-
    aplicarJugada(tauler(X),huma,Moviment,tauler(R)),
    Pp is Profunditat -1,
    minmax(tauler(R),Pp,cpu,PuntuacioMINMAX,Mp),
    %profunditat(Profunditat),
    %format("bp moviment: ",[]),
    %format("~w  ", [Moviment]),
    %format("menor valor: ",[]),
    %format("~w~n ", [PuntuacioMINMAX]),
    PuntuacioMINMAX < ValorActual,
    buscarPitjorMoviment(tauler(X),Profunditat,huma,Moviments,PuntuacioMINMAX,ValorEscollit,Moviment,MovEscollit), !.
buscarPitjorMoviment(tauler(X),Profunditat,huma,[Moviment|Moviments],ValorActual,ValorEscollit,MovActual,MovEscollit) :-
    aplicarJugada(tauler(X),huma,Moviment,tauler(R)),
    Pp is Profunditat -1,
    minmax(tauler(R),Pp,cpu,PuntuacioMINMAX,Mp),
    %profunditat(Profunditat),
    %format("bp xmoviment: ",[]),
    %format("~w  ", [Moviment]),
    %format("valor actual: ",[]),
    %format("~w~n ", [PuntuacioMINMAX]),
    PuntuacioMINMAX >= ValorActual,
    buscarPitjorMoviment(tauler(X),Profunditat,huma,Moviments,ValorActual,ValorEscollit,MovActual,MovEscollit).
%buscarPitjorMoviment(tauler([[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,x,-,-,-,-,-],[-,x,-,-,-,-,-],[-,x,-,-,-,-,-],[-,o,-,-,-,-,-]]),1,huma,[1,2,3,4,5,6,7],10,P,M,MR).
%buscarPitjorMoviment(tauler([[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,x,-,-,-,-,-],[-,x,-,-,-,-,-],[-,x,-,-,-,-,-],[-,o,-,-,-,-,-]]),1,huma,[2,3,4,5,6,7],0,P,1,MR).
%buscarPitjorMoviment(tauler([[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,x,-,-,-,-,-],[-,x,-,-,-,-,-],[-,x,-,-,-,-,-],[-,o,-,-,-,-,-]]),1,huma,[3,4,5,6,7],-1,P,2,MR).