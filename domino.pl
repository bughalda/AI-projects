%%%%DOMIAN%%%%%
%Este proyecto simula una inteligencia artificial para jugar dominó
%Este proyecto fue el ganador del concurso interno de la Universidad, en el que se 
%compitió contra otros proyectos programados por otros estudiantes del ITAM.
%Autores:
%Martin Fabre Garcia 
%Aldahir López Navarrete 
%Omar de Jesus Cruz Gloria 
%Angel Enrique Alvarado Ramos 


:- dynamic ficha/2.
:- dynamic cantNum/2.
:- dynamic extremos/2.
:-dynamic fichasMano/2.
:- dynamic cantMano/2.
:- dynamic fichasUsables/2.
:-  dynamic cantOp/1.
:-  dynamic turno/1.

ficha([0,0], 1).
ficha([0,1], 2).
ficha([0,2], 3).
ficha([0,3], 4).
ficha([0,4], 5).
ficha([0,5], 6).
ficha([0,6], 7).
ficha([1,1], 8).
ficha([1,2], 9).
ficha([1,3], 10).
ficha([1,4], 11).
ficha([1,5], 12).
ficha([1,6], 13).
ficha([2,2], 14).
ficha([2,3], 15).
ficha([2,4], 16).
ficha([2,5], 17).
ficha([2,6], 18).
ficha([3,3], 19).
ficha([3,4], 20).
ficha([3,5], 21).
ficha([3,6], 22).
ficha([4,4], 23).
ficha([4,5], 24).
ficha([4,6], 25).
ficha([5,5], 26).
ficha([5,6], 27).
ficha([6,6], 28).

cantNum(0,8).
cantNum(1,8).
cantNum(2,8).
cantNum(3,8).
cantNum(4,8).
cantNum(5,8).
cantNum(6,8).

cantMano(0,0).
cantMano(1,0).
cantMano(2,0).
cantMano(3,0).
cantMano(4,0).
cantMano(5,0).
cantMano(6,0).

cantOp(7).


cambiarContGeneral([]):-!.

cambiarContGeneral([X|Y]):-
    cantNum(X,Z), %consulta la cantidad actual del primer número
    NuevaZ is Z - 1, %disminuye su contador
    retract(cantNum(X,Z)), %elimina el predicado con la cantidad errónea
    asserta(cantNum(X,NuevaZ)), %actualiza la base de conocimiento con la nueva cantidad
    cambiarContGeneral(Y),!. %llamada recursiva con la cola de la lista (el segundo número)

cambiarExtremoIzq(X):-
    extremos(_,Y), % busca el extremo derecho actual
    retract(extremos(_,Y)), %elimina el predicado anterior
    asserta(extremos(X,Y)). %actualiza la base de conocimiento con el extremo izquierdo actual y el extremo derecho antiguo

cambiarExtremoDer(X):-
    extremos(Y,_), %busca el extremo izquierdo actual
    retract(extremos(Y,_)), %elimina el predicado anterior
    asserta(extremos(Y,X)). %actualiza la base de conocimiento con el extremo derecho actual y el extremo izquierdo antiguo

cambiar_contMano_come([]):-!.

cambiar_contMano_come([X|Y]):-
    cantMano(X,Z), %Obtiene el valor actual del contador para el primer número
    NuevaZ is Z + 1, %Aumenta el contador de dicho número
    retract(cantMano(X,_)), %Elimina el conocimiento antiguo
    asserta(cantMano(X,NuevaZ)), %Actualiza la base de conocimiento con el nuevo contador
    cambiar_contMano_come(Y). %Llamada recursiva para repetir el proceso con el segundo número de la ficha

cambiar_contMano_tira([]):-!.

cambiar_contMano_tira([X|Y]):-
    cantMano(X,Z), %Obtiene el valor actual del contador del primer número
    NuevaZ is Z - 1, %Disminuye el contador
    retract(cantMano(X,Z)), %Elimina el conocimiento antiguo
    asserta(cantMano(X,NuevaZ)), %Actualiza la base de conocimiento con el nuevo contador

    cambiar_contMano_tira(Y). %Llamada recursiva para repetir el proceso con el segundo número

come(Ficha):-
    ficha(Ficha,Y), %Busca la ficha (con su índice) en la base de conocimiento general
    assertz(fichasMano(Ficha, Y)), %Agrega la ficha (con su índice) a su mano
    cambiar_contMano_come(Ficha), %Cambia el contador de las fichas que están en su mano
    retract(ficha(_,Y)),!, %Elimina la ficha del pozo a partir de su índice
    cambiarContGeneral(Ficha). %Cambia el contador de las fichas del pozo

tiraFicha(Ficha,X,'derecho'):-
    fichasMano(Ficha,Y), %Busca la ficha (con su índice) en la base de conocimiento general
    retract(fichasMano(_,Y)), %Elimina la ficha de la mano de Dominique
    cambiarExtremoDer(X), %Actualiza el extremo derecho
    cambiar_contMano_tira(Ficha),!. %Actualiza el contador de la mano de Dominique

tiraFicha(Ficha,X,'izquierdo'):-
    fichasMano(Ficha,Y), %Busca la ficha (con su índice) en la base de conocimiento general
    retract(fichasMano(_,Y)),  %Elimina la ficha de la mano de Dominique
    cambiarExtremoIzq(X),  %Actualiza el extremo izquierdo
    cambiar_contMano_tira(Ficha),!.  %Actualiza el contador de la mano de Dominique

primerTiro(Ficha, X, Y):-
    %fichasMano(Ficha,Ind), %Busca la ficha (con su índice) en la base de conocimiento general
    retract(fichasMano(Ficha,_)), %Elimina la ficha de la mano de Dominique
    asserta(extremos(X,Y)), %Inserta el predicado "extremos" en la base de conocimiento
    cambiar_contMano_tira(Ficha). %Cambia el contador de la mano de Dominique

getFicha(X, Ind, Ficha):-
    fichasMano([X|_],Ind), %Busca el índice de la ficha que contenga el número en el lado izquierdo
    fichasMano(Ficha, Ind). %regresa la ficha completa

getFicha(X, Ind, Ficha):-
    fichasMano([_|[X|_]],Ind), %busca el índice de la ficha que contenga el npumero en el lado derecho
    not(esMula(Ind)), %si esa no es una mula, puede continuar
    fichasMano(Ficha, Ind). %regresa la ficha completa

valorMin(X,Y,X,L):-
    X<Y, %Evalúa si el primer número es menor al segundo
    L = 'izquierdo', !. %Si es menor el primero, entonces nos regresa el lado izquierdo

% Definición de predicado para obtener el valor mínimo entre dos valores
% También se especifica el lado a usar en el caso de empate
valorMin(_,Y,Y,L):-
    L = 'derecho',!. %Regresa lado derecho

% Definición de predicados para obtener el extremo que se debe usar
% Se especifica el lado donde se encuentra el extremo
getExtremos(X,_,'izquierdo',X):-!. %Regresa extremo izquierdo
getExtremos(_,Y,'derecho',Y):-! .%Regresa extremo derecho


calcUtilidad(Ext_izq,Ext_der,Utilidad,Lado,Usable):-
    cantMano(Ext_izq,CantIzq), %Obtiene la cantidad de fichas que tenemos de cada extremo
    cantMano(Ext_der,CantDer),  %Obtiene la cantidad de fichas que tenemos de cada extremo
    CantIzq_posible_disponible is 8-CantIzq,
    CantDer_posible_disponible is 8-CantDer, %Obtiene la cantidad de fichas que hay en general para cada extremo
    Pei is CantIzq_posible_disponible/8, %Obtiene la probabilidad de que el oponente tenga una ficha con ese número
    Ped is CantDer_posible_disponible/8,
    valorMin(Pei,Ped,Min,Lado), %Obtiene el valor mínimo de las probabilidades y el lado para minimizar la ganancia del oponente
    Utilidad = Min,
    getExtremos(Ext_izq,Ext_der,Lado,Usable). %Nos dice que extremo debemos jugar

posiblesFichas(X,Y,Lado,ExtElegido):-
    cantMano(X,CantX),
    cantMano(Y,CantY), %Obtiene la cantidad de fichas que Dominique tiene para cada extremo
    CantX =\= 0,
    CantY =\= 0, %Si tiene fichas para ambos extremos, entonces continúa
    calcUtilidad(X,Y,_,Lado,ExtElegido), %Calcula la utilidad de cada extremo y regresa el que se debe usar
    getFicha(ExtElegido,Ind,Ficha), %busca las fichas que están en nuestra mano para dicho extremo
    asserta(fichasUsables(Ficha,Ind)),!. %agrega las fichas a la base de conocimiento necesaria

posiblesFichas(X,Y,Lado,Y):-
    cantMano(X,CantX),
    cantMano(Y,CantY), %Obtiene la cantidad de fichas que Dominique tiene para cada extremo
    CantX = 0,
    CantY =\= 0, %Si no tiene fichas para el lado izquierdo pero sí para el derecho, continúa
    getFicha(Y,Ind,Ficha), %busca las fichas que se tienen para ese lado
    Lado = 'derecho',
    asserta(fichasUsables(Ficha,Ind)),!. %agrega las fichas a la base de conocimiento necesaria

posiblesFichas(X,Y,Lado,X):-
    cantMano(X,CantX),
    cantMano(Y,CantY),  %Obtiene la cantidad de fichas que Dominique tiene para cada extremo
    CantX =\= 0,
    CantY = 0, %Si no tiene fichas para el lado derecho pero sí para el izquierdo, continúa
    getFicha(X,Ind,Ficha),
    Lado = 'izquierdo',
    asserta(fichasUsables(Ficha,Ind)),!.
    
% esta función obtiene un valor para manejar internamente las fichas (declarados al inicio)
valorFicha([X, Y], Valor) :- Valor is X + Y.

%esta función compara 2 fichas para ver cual es la mayor
comparaFichas(Ficha1, Ficha2, FichaMayor) :-
    valorFicha(Ficha1, Valor1),
    valorFicha(Ficha2, Valor2),
    (Valor1 >= Valor2 -> FichaMayor = Ficha1
    ; FichaMayor = Ficha2).


%esta funcion selecciona la ficha mayor y la elimina de las fichas usables
selectFicha(FichaMayor) :-
    findall(Ficha, fichasUsables(Ficha, _), ListaFichas),
    foldl(comparaFichas, ListaFichas, [0, 0], FichaMayor),
    retract(fichasUsables(FichaMayor, _)).

%estas funciones definen cual es el nuevo extremo durante el juego
setExtremo([X|[Y|_]],X,Y):-!.
setExtremo([X|[Y|_]],Y,X):-!.

%esta funcion sirve para representar el movimiento en el que el oponente come
comeOp(Cant):-
    cantOp(X), %Busca la cantidad actual de fichas que tiene el oponente
    Nueva is X + Cant, %Le suma la cantidad de fichas que el oponente comió
    retract(cantOp(X)), %Elimina la cantidad antigua de la base de conocimiento
    asserta(cantOp(Nueva)). %Actualiza la base de conocimiento con la nueva cantidad

%esta funcion sirve para representar el movimiento en el que el oponente tira
comeOp(Cant):-
    cantOp(X), %Busca la cantidad de fichas que tiene el oponente
    Nueva is X + Cant, %Suma la cantidad que comio el oponente a la variable
    retract(cantOp(X)), %Quita la cantidad antigua de la base de conocimiento
    asserta(cantOp(Nueva)). %Inserta la nueva cantidad a la base de conocimiento

quitaOp:-
    cantOp(X), %Busca la cantidad de fichas del oponente
    Nueva is X - 1, %Resta en 1 la cantidad
    retract(cantOp(X)), %Quita la cantidad anterior
    asserta(cantOp(Nueva)). %Inserta la nueva cantidad a la base de conocimiento

tiraFichaOp(Ficha,X,'der'):-
    retract(ficha(Ficha,_)), %Quita la ficha de la base de conocimiento general
    cambiarExtremoDer(X), %Cambia el extremo derecho
    quitaOp, %Quita una ficha al contador de la mano del oponente
    cambiarContGeneral(Ficha),!. %Cambia el contador

tiraFichaOp(Ficha,X,'izq'):-
    retract(ficha(Ficha,_)), %Quita la ficha de la base de conocimiento general
    cambiarExtremoIzq(X), %Cambia el extremo izquierdo
    quitaOp, %Quita una ficha al contador de la mano del oponente
    cambiarContGeneral(Ficha),!. %cambia el contador

primerTiroOp(Ficha, X, Y):-
    retract(ficha(Ficha,_)), %Quita la ficha de la base de conocimiento general
    asserta(extremos(X,Y)), %Agrega el predicado de "extremos" con los números de la ficha
    quitaOp, %Quita una ficha al contador de la mano del oponente
    cambiarContGeneral(Ficha). %Cambia el contador general

esMula(Ind):-
    Ind==1;
    Ind==8;
    Ind==14;
    Ind==19;
    Ind==23;
    Ind==26;
    Ind==28.

terminaJuego:-
    cantOp(0),!, %Si las fichas del oponente son 0, entonces se realiza un corte
    write('Perdiste.'). %Hemos perdido

terminaJuego:-
    not(fichasMano(_,_)),!, %Si nuestras fichas son 0, entonces se realiza un corte
    write('¡Ganaste!'). %Hemos ganado
    %Res is 1,!.

terminaJuego:-
    extremos(X,X), %Revisa si los extremos sean iguales.
    cantMano(X,0), %Revisa si no tenemos ese número en la mano
    cantNum(X,0),!, %Revisa si en la base de conocimiento general no hay ese número
    write('Se cerró el juego. Empate'). %Empatamos
    %Res is 0,!.

jugadaDom('comer'):-
    writeln('Describe la ficha que comiste'),
    read(X), read(Y), %Describimos la nueva ficha
    come([X,Y]), %Actualiza las bases de conocimiento 
    jugada('Yo'),!. %Regresa al método general de la jugada de Domian.

jugadaDom('pasar'):-
    retract(turno(_)),
    asserta(turno('Op')),!.

jugada('Yo'):-
   extremos(X,Y), %Revisa los extremos del tablero
   not(posiblesFichas(X,Y,_,_)), %Revisa si no hay fichas posibles para tirar
   nl,
   writeln('JUGADOR: '),
   writeln('Tenemos que comer o pasar.'),
   writeln('Escribe "comer" o "pasar" basado en la acción que tomaste'),
   read(Accion),
   jugadaDom(Accion),!. %Realiza la siguiente jugada acorde con la acción que le indicamos

jugada('Yo'):-
   nl,
   writeln('TURNO JUGADOR:'),
   extremos(X,Y),!, %Obtiene los extremos del tablero
   posiblesFichas(X,Y,Lado,Usa), %Guarda las fichas y nos dice en que extremo jugar
   selectFicha(Ficha), %escoge la mejor eleccion de ficha
   write('La ficha que vas a tirar es: '),write(Ficha), 
   nl,
   write('Insértala en el lado '), write(Lado), 
   nl,
   setExtremo(Ficha,Usa,NuevoExt),!,
   tiraFicha(Ficha,NuevoExt,Lado), %Tira la ficha y cambia las bases de conocimiento correspondientes
   retract(turno(_)), %Elimina el turno actual
   asserta(turno('Op')),!, %Actualiza el turno
   retract(fichasUsables(_,_)),!. %Elimina todas las fichas de la base de conocimiento para volverla a usarla

jugada('Op'):-
   nl,
   writeln('TURNO OPONENTE:'),
   writeln('Si comió, escribe "come"; de otra forma, escribe "tira" o "pasa" (según corresponda)'),
   read(Accion), %Accion = acción que realizó el oponente
   jugadaOp(Accion),!. % Según la acción que le digamos, realiza algo diferente

jugadaOp('come'):-
    writeln('¿Cuántas fichas comió?'),
    read(Cant),%Cant = cantidad de fichas que comió el oponente
    comeOp(Cant), %Actualiza las bases de conocimiento necesarias
    writeln('Escribe la siguiente acción del oponente ("tira" o "pasa")'),
    read(Accion), %Accion = siguiente acción del oponente
    jugadaOp(Accion),!. %Llama al predicado de jugadaOp para ralizar la siguiente acción

jugadaOp('pasa'):-
    retract(turno(_)), % Elimina el turno
    asserta(turno('Yo')),!. % cambia el turno para que siga Domian

jugadaOp('tira'):-
    writeln('Describe la ficha que tiró'),
    read(X),
    read(Y), %Ingresamos los números de la ficha que tiró el oponente, 
    %siempre dando el número menor primero (es decir, Y>=X)
    writeln('¿De qué lado tiró?'),
    writeln('Escribe "der" o "izq"'),
    read(Lado), %Lado = el lado en el que tiró el oponente
    %En este caso 'der' = derecho e 'izq' = izquierdo
    writeln('¿Cuál es el nuevo extremo en donde tiraron la ficha?'),
    read(NuevoExt), %agregamos el nuevo extremo
    tiraFichaOp([X,Y],NuevoExt,Lado), %Actualiza las bases de conocimiento necesarias
    retract(turno(_)), %Elimina el turno
    asserta(turno('Yo')),!. %Cambia el turno

empieza2:-
    nl,
    writeln('TURNO OPONENTE: '),
    writeln('¿Cuál fue su primera ficha?'),
    read(X),
    read(Y), %Ingresamos los números de la ficha que se tiró,
    %siempre dando el número menor primero (es decir, Y>=X)
    primerTiroOp([X,Y], X, Y), %Actualiza las bases de conocimiento necesarias
    asserta(turno('Yo')),
    !. %Iniciamos con el turno siendo de Domian; el oponente ya tiró la primera ficha

empieza1:-
    nl,
    writeln('TURNO JUGADOR: '),
    writeln('¿Cuál fue esa primera ficha?'),
    read(X),
    read(Y),  %se indica la ficha en juego primero menor despues mayor
    primerTiro([X,Y], X, Y), %Se actualizan las bases de conocimiento
    asserta(turno('Op')),
    !. %Se agrega en turno el turno del oponente ya que Domián ya tiro


comeInicial(0):-!.

comeInicial(Cont):-
    writeln('Describe la ficha tomando en cuenta el orden de los índices;'),
    writeln('es decir, PRIMERO ingresa EL número MÁS PEQUEÑO de la ficha'),
    read(X),
    read(Y), %se mete la ficha comida
    come([X,Y]), %actualiza las bases relacionadas con las fichas
    NuevoCont is Cont - 1, %resta uno a las fichas por comer
    comeInicial(NuevoCont),
    !. %reinicia con el nuevo contador


jugar(Turno):-
   jugada(Turno), %Se dice de quien es el turno
   not(terminaJuego), %checa si el juego se acabó después del turno
   turno(NuevoTurno), %si todavía hay juego checa de quién es el turno
   jugar(NuevoTurno). %permite jugar al jugador sigueinte


iniciaJuego:-
   writeln('Inicia comiendo 7 fichas'),
   comeInicial(7),
   writeln('¿Quién tuvo la mula/ficha más grande? ("yo"/"op")'),
   %escribimos 'yo' u 'op'
   read(Empieza), %le decimos quién empieza
   ( Empieza == yo -> empieza1
   ; Empieza == op -> empieza2
   ),
   turno(Turno), %checa de quién es el nuevo turno
   jugar(Turno). %permite el juego 
