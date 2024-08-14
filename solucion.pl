% La solución va acá. Éxitos!
% punto 1
juega(ana,romanos,herreria).
juega(ana,romanos,forja).
juega(ana,romanos,emplumado).
juega(ana,romanos,lamina).
juega(beto,incas,herreria).
juega(beto,incas,forja).
juega(beto,incas,fundicion).
juega(carola,romanos,herreria).
juega(dimitri,romanos,herreria).
juega(dimitri,romanos,herreria).
juega(dimitri,romanos,fundicion).

jugador(Jugador):-
    juega(Jugador,_,_).

civilizacion(Jugador,Civilizacion):-
    juega(Jugador,Civilizacion,_).

% no se agrega a la base de conocimientos a elsa como jugadora ya que no juega

% punto 2
expertoEnMetales(Jugador):-
    desarrolloHerreriayForja(Jugador),
    tieneTecnologia(Jugador,fundicion).

expertoEnMetales(Jugador):-
    desarrolloHerreriayForja(Jugador),
    civilizacion(Jugador,romanos).

desarrolloHerreriayForja(Jugador):-
    jugador(Jugador),
    tieneTecnologia(Jugador,herreria),
    tieneTecnologia(Jugador,forja).

% punto 3

% punto 4

% punto 5

% punto 6
% tiene(Jugador,UnaUnidad).
tiene(ana,jinete(caballo)).
tiene(ana,piquero(1,conEscudo)).
tiene(ana,piquero(2,sinEscudo)).
tiene(beto,campeon(100)).
tiene(beto,campeon(80)).
tiene(beto,piquero(1,conEscudo)).
tiene(beto,jinete(camello)).
tiene(carola,piquero(3,sinEscudo)).
tiene(carola,piquero(2,conEscudo)).
% como Dimitri no tiene unidades no es necesario agregar a la base de conocimiento

% punto 7

% punto 8

% punto 9

% punto 10
% A
% dependencia(UnaTecnologia,DependienteTecnologia)
dependencia(_,herreria).
dependencia(herreria,emplumado).
dependencia(herreria,forja).
dependencia(herreria,lamina).
dependencia(emplumado,punzon).
dependencia(forja,fundicion).
dependencia(fundicion,horno).
dependencia(lamina,malla).
dependencia(malla,placas).
dependencia(_,molino).
dependencia(molino,collera).
dependencia(collera,arado).

tecnologia(Tecnologia):-
    dependencia(_,Tecnologia).
 
arbolDeTecnologia(PrimeraTecnologia,SegundaTecnologia):-
    tecnologia(PrimeraTecnologia),
    tecnologia(SegundaTecnologia),
    dependencia(PrimeraTecnologia,SegundaTecnologia).

arbolDeTecnologia(PrimeraTecnologia,SegundaTecnologia):-
    dependencia(PrimeraTecnologia,SegundaTecnologia),
    arbolDeTecnologia(PrimeraTecnologia,SegundaTecnologia).

% B
tieneTecnologia(Jugador,Tecnologia):-
    juega(Jugador,_,Tecnologia).

puedeDesarrollar(Jugador,Tecnologia):-
    jugador(Jugador),
    tecnologia(Tecnologia),
    forall((juega(Jugador,_, UnaTecnologia),dependencia(UnaTecnologia,Tecnologia)), not(tieneTecnologia(Jugador,Tecnologia))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%   TEST   %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(experto_En_metales).

test("personas que son expertos en metales",set(Personas=[ana,beto])) :-
    expertoEnMetales(Personas).

test("personas que no son expertos en metales") :-
    not(expertoEnMetales(carola)),not(expertoEnMetales(dimitri)).

:- end_tests(experto_En_metales).

%%%% FALTA TEST DE INTEGRANTE

:- begin_tests(jugador_puede_desarrollar_una_tecnologia).

test("jugador que puede desarrollar una tecnologia sin dependencia",nondet) :-
    puedeDesarrollar(beto,molino).

test("jugador no puede desarrolar una tecnologia que ya tiene", fail) :-
    puedeDesarrollar(beto,herreria).

test("jugador no puede desarrolar una tecnologia que ya tiene", nondet) :-
    puedeDesarrollar(ana,fundicion).

:- end_tests(jugador_puede_desarrollar_una_tecnologia).