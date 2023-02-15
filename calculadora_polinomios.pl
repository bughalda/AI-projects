%Proyecto Inteligencia Artificial 14/02/23

%Estas funciones se logran con recursividad por lo que los casos base
% estaran marcados con un "Si..."
%----------------------------------------------------------------------
%	 Suma de polinomios
%  Esto se logra haciendo una suma de elementos entre 2 listas
suma([],[],[]).%Si ambas listas estan vacias resultado es vacio
suma([],P2,P2).%Si P2 es de mayor longitud que P1 la base del resultado sera P2
suma(P1,[],P1).%Si P1 es de mayor longitud que P2 la base del resultado sera P1

suma([P1|Rest1],[P2|Rest2],[RestSuma|Suma_Res]):- %Se suman las cabezas y
        suma(Rest1,Rest2,Suma_Res),    % se usa el resto de las listas
        RestSuma is P1+P2.    %recursivamente, poniendo como cabeza el
                              %res de cada una
%-----------------------------------------------------------------------
%  Resta de polinomios
%  El segundo polinomio se transforma a negativo para después hacer una suma

resta([],[],[]). % Si ambos polinomios tienen 0 términos
resta([],P2,-P2). % Si el polinomio 2 tiene más términos que el polinomio 1
resta(P1,[],P1). %Si el polinomio 1 tiene más términos que el polinomio 2
resta(P1,P2,RespR):-
      polinomio_negativo(P2,P2_negativo),
      %Se convierte el polimio 2 a negativo para hacer una suma
	suma(P1,P2_negativo,RespR).
      %Se aplica la suma entre los dos polinomio como en el caso anterior

polinomio_negativo([],[]). %si la lista esta vacia se regresa una lista vacia
polinomio_negativo([Poli_n|Poli_r], [Poli_negativo|Poli_negativo_r]):-
        %en caso contrario se multiplica cada elemento por -1 y
        %se guarda en la nueva lista
        Poli_negativo is Poli_n*(-1),
        polinomio_negativo(Poli_r,Poli_negativo_r).


%-------------------------------------------------------------------
% Ayuda a nivela_grado agragando ceros
agrega_ceros(_,0,[]).%Si el grado es cero, regresa una lista vacia
agrega_ceros(0,1,[0]). %Si el grado es uno, se regresa una lista con un solo cero.
agrega_ceros(0,Gr,[0|Cola]):- %Caso grados >1
        Gr>0, %Caso base.
        New_Gr is Gr-1,
        agrega_ceros(0,New_Gr,Cola). %Recursivamente se agregan los ceros a la lista nueva



% Predicado que añade 0 por la izquierda en la lista para representar
% la multiplicacion, es decir, que se aumenta de grado.
nivela_grado(Polinomio_a_cambiar,0,Polinomio_a_cambiar). %Si el grado es cero, se regresa la lista dada.
nivela_grado(Polinomio_a_cambiar,Gr,Polinomio_cambiado):-
        Gr>0,                     %Crea la lista con los ceros necesarios
        agrega_ceros(0,Gr,Ceros), %  para balancear el grado.
        append(Ceros,Polinomio_a_cambiar,Polinomio_cambiado).%deja los ceros a
                                         %la izquierda, y une listas
% --------------------------------------------------------------------------
% Multiplica un escalar por un polinomio.
% K = escalar
multiplicacion_por_escalar(0,_,[]). %Si el escalar es 0 se regresa una lista vacia
multiplicacion_por_escalar(_,[],[]). %Si el polinomio es 0 es decir una lista vacia se regresa igual una lista vacia.
multiplicacion_por_escalar(K,[Primer_elemento|Restante],[Res_primer_elemento|Cola]):-
        Res_primer_elemento is K*Primer_elemento,
        multiplicacion_por_escalar(K,Restante,Cola). %Los coeficientes
             %multiplicados por el escalar se guardan en la nueva lista

% Multiplica un polinomio por un monomio.

multiplicacion(_,0,_,[]).%Si el coeficiente es 0 regresa una lista vacía.
multiplicacion(_,_,[],[]). %Si el polinomio está vacío regresa una lista vacía.
multiplicacion(Gr,Coef,P,Res):-
        multiplicacion_por_escalar(Coef,P,Res1),%Se multiplican los coeficientes
        %             por el escalar del monommio y se agrega a una lista
        nivela_grado(Res1,Gr,Res). %Se aumenta el grado aumentando un 0

% Multiplicación de polinomios.

multiplicacion_polinomios([],_,[]). %Si el primer polinomio esta vacío regresa una lista vacía.
multiplicacion_polinomios(_,[],[]). %Si el segundo polinomio esta vacío regresa una lista vacía.
multiplicacion_polinomios(P1,P2,Res):-
        multiplicacion_polinomios_private(0,P1,P2,Res).
multiplicacion_polinomios_private(_,[],_,[]). %Si el primer polinomio esta vacío regresa una lista vacía.
multiplicacion_polinomios_private(_,_,[],[]). %Si el segundo polinomio esta vacío regresa una lista vacía.
multiplicacion_polinomios_private(Gr,[Cabeza|Cola],P2,Res):-
        Gr1 is Gr+1,
        multiplicacion_polinomios_private(Gr1,Cola,P2,Res1), %Se baja el grado y
                                  %         se multiplica por el restante
        multiplicacion(Gr,Cabeza,P2,Res2),
        suma(Res1,Res2,Res).
% ------------------------------------------------------------------------
% Juntar una lista en otra
concatenacion([],L2,L2). %Si la Lista 1 esta vacia se
%                         regresa la lista 2 como la nueva
concatenacion([Cabeza|L1],L2,[Cabeza|L3]) :- concatenacion(L1,L2,L3).
% Se ponen los elementos de la lista 1 y cuando se vacia alcanza el
% caso base

% Invierte los elementos de una lista
lista_invertida([],[]). % caso base que regresa una lista vacia si la
                        % original esta vacia
lista_invertida([Cabeza|Cola],Lista_nueva) :-
   lista_invertida(Cola, Cola_nueva),
   concatenacion(Cola_nueva, [Cabeza],Lista_nueva).
% -----------------------------------------------------------------------
% Composición de polinomios P1(P2(x)) se evalua un polinomio dentro de
% otro:
composicion(P1,[],P1). %Si el polinomio 2 esta vacío se regresa el polinomio 1
composicion(P1,P2,Res):-
    lista_invertida(P1, P1_invertido),	% Se invierte el polinomio para poder
                                        % usar el método de Horner.
    composicion(P1_invertido,P2,[0],Res).
composicion([],_,Comp,Comp). % Si se termina de evaluar el primer polinomio
                             % se regresa la composicion.
composicion([X|P1],Y,W,Res):-
    multiplicacion_polinomios(Y,W,ProdYW),
    suma([X], ProdYW, Sum),
    composicion(P1,Y,Sum,Res).	% Recursivamente se evalúa B en P1 y se
                                % actualiza el polinomio auxiliar
%----------------------------------------------------------------------
% Evaluar polinomio
resuelve(Polinomio,X,Res):-
    resuelve_private(Polinomio,X,0,Res).
resuelve_private([],_,_,0). %corte cuando se acaban los elementos a evaluar
resuelve_private([P1|Rest1],X,0,Res):- % cuando el grado es 0 solo se suma
                    %la cabeza de la lista pues es el termino independiente
    resuelve_private(Rest1,X,1,Res1),
    Res is P1+Res1.
resuelve_private([P1|Rest1],X,Gr,Res):-
    Gr1 is Gr+1, %se avanza al siguiente elemento del polinomio
    resuelve_private(Rest1,X,Gr1,Res1), % se multiplica el coeficiente por la
    Resultado_evaluacion is P1*(X**Gr), % x dada elevada al grado
    Res is Resultado_evaluacion+Res1. % se suma el numero real a el resultado
                                      % que se tiene
% --------------------------------------------------------------------------
% Derivacion
derivada(Polinomio,Res):-
        derivada_private(0,Polinomio,Res). % se agrega un 0 para que el que realize la consulta no lo necesite y asi multiplicar el coeficiente de cada elemento por el grado que tenga
derivada_private(_,[],[]). % si se acaban los elementos a evaluar entonces se regresa una lista vacia y se permite que se realice el regreso
derivada_private(0,[_|Cola],Polinomio_derivado_un_gradomenor):- %con este metodo logramos que en la nueva lista los nuevos coeficientes concuerden con su nuevo grado ya que al derivar se resta uno
    derivada_private(1,Cola,Polinomio_derivado_un_gradomenor).
derivada_private(Gr,[P1|Rest1],[Nuevo|Resultado]):- %se multiplica el coeficiente por el grado que le toca y se guarda en la lista
    Gr1 is Gr+1,
    derivada_private(Gr1,Rest1,Resultado),
	  Nuevo is P1 * Gr.
% ------------------------------------------------------------------------
% Escribir los polinomios
escribe_polinomio(Polinomio) :-
        lista_invertida(Polinomio,Polinomio_invertido), %invierte lista para escribir de forma correcta
        length(Polinomio_invertido,L), %funcion lenght prediseñada
        L1 is L -1, %este sera el indice
        LA is L-1, %este sera nuestra guia para saber cual es el primer elemento
        escribe_polinomio_private(Polinomio_invertido, L1,LA). %predicado_aux
escribe_polinomio_private([0],_,0):- %si se pide escribir el pol 0 e regresa 0.
        write(0).
escribe_polinomio_private([], _,_). %si el polinomio es vacio se para con lo que se tenga.

escribe_polinomio_private([0|Rest1], Indice,LA) :- % si el elemento es 0 entonces
    Indice1 is Indice - 1,   %se reduce el indice y se pasa al siguiente elemento
    escribe_polinomio_private(Rest1, Indice1,LA).

escribe_polinomio_private([P1|_], 0,_):- %si el indice es 0 y el elemento es
        P1 > 0,                      %positivo, se escribe un mas y el elemento
        write('+'),write(P1).
escribe_polinomio_private([P1|_], 0,_):- %igual que el anterior solo si es menor a
        P1 < 0,                         %0 entonces solo se pone el elemento pues
        write(P1).                      %ya trae signo.
escribe_polinomio_private([P1|Rest1], Indice,LA) :- % si el elemnto es mayor a 0
    P1 > 0, P1 \= 1, %y distinto a uno, y su indice es uno entonces se verifica si
    Indice =:= 1, % es el primer elemento y si si se omite el signo sino se pone
    (   Indice =:= LA -> write(P1),write(x);write('+'),write(P1),write('x')),
    Indice1 is Indice - 1, %igual que arriba se baja el indice
    escribe_polinomio_private(Rest1, Indice1,LA).

escribe_polinomio_private([P1|Rest1], Indice,LA) :-
    P1 =:= 1, %este es igual al anterior solo que si su coeficiente es uno solo
    Indice =:= 1, %escribira x en vez de #x
    (   Indice =:= LA -> write(x);write('+'),write('x')),
    Indice1 is Indice - 1,
    escribe_polinomio_private(Rest1, Indice1,LA).

escribe_polinomio_private([P1|Rest1], Indice,LA) :-
    P1 > 0, P1 \= 1, %igual que hace 2 predicados, solo ahora se agrega el exp
    Indice > 1,   %usando el indice
    (Indice == LA -> write(P1),write('x^'),write(Indice);
               write('+'),write(P1),write('x^'),write(Indice)),
    Indice1 is Indice - 1,
    escribe_polinomio_private(Rest1, Indice1,LA).

escribe_polinomio_private([P1|Rest1], Indice,LA) :-
    P1 =:= 1, %mismo que anterior pero para cuando es 1 el coeficiente
    Indice > 1,(Indice == LA -> write(P1),write('x^'),write(Indice);write('+'),write(P1),write('x^'),write(Indice)),
    Indice1 is Indice - 1,
    escribe_polinomio_private(Rest1, Indice1,LA).

escribe_polinomio_private([P1|Rest1], Indice,LA) :-
    P1 < 0, P1 \= -1, %igual que anteriores pero cuando es negativo,
    Indice =:= 0, %al ser negativo siempre se pondra el signo este al principio
    write(P1),  %o al final
    Indice1 is Indice - 1,
    escribe_polinomio_private(Rest1, Indice1,LA).

escribe_polinomio_private([P1|Rest1], Indice,LA) :-
    P1 < 0, P1 \= -1, %igual que anterior solo que es para la x elevada a 1
    Indice =:= 1,
    write(P1),write('x'),
    Indice1 is Indice - 1,
    escribe_polinomio_private(Rest1, Indice1,LA).

escribe_polinomio_private([P1|Rest1], Indice,LA) :-
    P1 =:= -1, %similar a anterior pero cuando el coeficiente sea -1
    Indice =:= 1, %en vez de escribir solo el coeficiente se pone un menos a la x
    write('-'),write('x'),
    Indice1 is Indice - 1,
    escribe_polinomio_private(Rest1, Indice1,LA).

escribe_polinomio_private([P1|Rest1], Indice,LA) :-
    P1 < 0, P1 \= -1, % este no necesita if pues siempre se pone el signo al
    write(P1),write('x^'),write(Indice), %negativo, igual que arriba pero se
    Indice1 is Indice - 1, %se incluye exponente
    escribe_polinomio_private(Rest1, Indice1,LA).

escribe_polinomio_private([P1|Rest1], Indice,LA) :-
    P1 =:= -1, %lo mismo pero para cuando el coeficiente es -1
    Indice > 1, %se omite el coeficiente y se pone solo el signo
    write('-'),write('x^'),write(Indice),
    Indice1 is Indice - 1,
    escribe_polinomio_private(Rest1, Indice1,LA).

% ------------------------------------------------------------------------
% Pruebas a realizar
 % El resultado de ejecutar el predicado polynomial deberá ser:
 %  zero(x)     = 0
 % p(x)        = 4x^3 + 3x^2 + 2x + 1
 %  q(x)        = 3x^2 + 5
 %  p(x) + q(x) = 4x^3 + 6x^2 + 2x + 6
 %  p(x) * q(x) = 12x^5 + 9x^4 + 26x^3 + 18x^2 + 10x + 5
 %  p(q(x))     = 108x^6 + 567x^4 + 996x^2 + 586
 %  0 - p(x)    = -4x^3 - 3x^2 - 2x - 1
 %  p(3)        = 142
 %  p'(x)       = 12x^2 + 6x + 2
 %  p''(x)      = 24x + 6
 %
% -----------------------------------------------------------------------
polynomial:-
    Cero = [0],
    P = [1,2,3,4],
    Q = [5,0,3],
    write('zero(x)     = '),
    escribe_polinomio(Cero),nl,
    write('p(x)        = '),
    escribe_polinomio(P),nl,
    write('q(x)        = '),
    escribe_polinomio(Q),nl,
    write('p(x) + q(x) = '),
    suma(P,Q,Suma),
    escribe_polinomio(Suma),nl,
    write('p(x) * q(x) = '),
    multiplicacion_polinomios(P,Q,Mult),
    escribe_polinomio(Mult),nl,
    write('p(q(x))     = '),
    composicion(P,Q,Comp),
    escribe_polinomio(Comp),nl,
    write('0 - p(x)    = '),
    resta(Cero,P,Resta),
    escribe_polinomio(Resta),nl,
    write('p(3)        = '),
    resuelve(P,3,P_evaluada),
    writeln(P_evaluada),
    write("p'(x)       = "),
    derivada(P,Derivada),
    escribe_polinomio(Derivada),nl,
    write("p''(x)      = "),
    derivada(Derivada,Segunda_derivada),
    escribe_polinomio(Segunda_derivada),nl,
    !.
