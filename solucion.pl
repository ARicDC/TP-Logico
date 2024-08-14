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
juega(dimitri,romanos,fundicion).

jugador(Jugador):-
    distinct(Jugador, juega(Jugador,_,_)).

civilizacion(Jugador,Civilizacion):-
    distinct(Civilizacion ,juega(Jugador,Civilizacion,_)).

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
civilizacionPopular(Civilizacion):- 
    cantJugadores(Civilizacion, CantidadJugadores),
    CantidadJugadores > 1.

cantJugadores(Civilizacion, CantidadJugadores):- 
    civilizacion(_,Civilizacion),
    findall(Jugador, distinct(Jugador, juega(Jugador, Civilizacion,_)), Jugadores),
    length(Jugadores, CantidadJugadores).

% punto 4
tieneAlcanceGlobal(Tecnologia) :-
    tecnologia(Tecnologia), 
    forall(jugador(Persona), juega(Persona,_, Tecnologia)).

% punto 5
esLider(Civilizacion):-
    civilizacion(_,Civilizacion),
    findall(Tecnologia,juega(_,Civilizacion,Tecnologia),Tecnologias),
    forall(alcanzo(_,Tecnologia), member(Tecnologia,Tecnologias)).

alcanzo(Civilizacion,Tecnologia):-
    juega(_,Civilizacion,Tecnologia).


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
cantidadVida(jinete(camello),80).
cantidadVida(jinete(caballo),90).
cantidadVida(campeon(Vida),Vida).
cantidadVida(piquero(1,sinEscudo),50).
cantidadVida(piquero(2,sinEscudo),65).
cantidadVida(piquero(3,sinEscudo),70).
cantidadVida(piquero(Nivel,conEscudo),Vida):-
    cantidadVida(piquero(Nivel,sinEscudo),VidaSegunNivel),
    Vida is VidaSegunNivel + VidaSegunNivel * 0.1. 

conMasVida(Jugador,Unidad):-
    tiene(Jugador,Unidad),
    cantidadVida(Unidad,Vida),
    forall((tiene(Jugador,OtraUnidad),cantidadVida(OtraUnidad,OtraVida),Unidad \= OtraUnidad),Vida > OtraVida).

% punto 8
ventaja(jinete(_),campeon(_)).
ventaja(campeon(_),piquero(_,_)).
ventaja(piquero(_,_),jinete(_)).
ventaja(jinete(camello),jinete(caballo)).

leGana(Unidad,OtraUnidad):-
    ventaja(Unidad,OtraUnidad).

leGana(Unidad,OtraUnidad):-
    not(ventaja(OtraUnidad,Unidad)),
    tieneMayorVida(Unidad,OtraUnidad).

tieneMayorVida(Unidad,OtraUnidad):-
    cantidadVida(Unidad,Vida),
    cantidadVida(OtraUnidad,OtraVida),
    Unidad \= OtraUnidad,
    Vida > OtraVida.

% punto 9
puedeSobrevivir(Jugador):-
    jugador(Jugador),
    findall(Unidad,(tiene(Jugador,Unidad),piqueroConEscudo(Unidad)),PiquerosConEscudo),
    findall(Unidad,(tiene(Jugador,Unidad),piqueroSinEscudo(Unidad)),PiquerosSinEscudo),
    length(PiquerosConEscudo,CantidadDePiquerosConEscudo),
    length(PiquerosSinEscudo,CantidadDePiquerosSinEscudo),
    CantidadDePiquerosConEscudo > CantidadDePiquerosSinEscudo.
    
piqueroSinEscudo(piquero(_,sinEscudo)).
piqueroConEscudo(piquero(_,conEscudo)).

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
    distinct(Tecnologia, dependencia(_,Tecnologia)).
 
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

%--------------------------------------------------------------------------------------------------------------
%--------------------------------------------------------------------------------------------------------------

:- begin_tests(civilizacion_popular).

test("civilizacion es popular",nondet) :-
    civilizacionPopular(romanos).

test("civilizacion no es popular", fail) :-
    civilizacionPopular(incas).

:- end_tests(civilizacion_popular).

%--------------------------------------------------------------------------------------------------------------
%--------------------------------------------------------------------------------------------------------------

:- begin_tests(tiene_alcance_global).

test("tecnologia con alcance global") :-
    tieneAlcanceGlobal(herreria).

test("tecnologia no tiene alcance global", set(Tecnologia = [forja,emplumado,lamina,fundicion,punzon,horno,malla,placas,molino,collera,arado]), fail) :-
    tieneAlcanceGlobal(Tecnologia).

:- end_tests(tiene_alcance_global).

%--------------------------------------------------------------------------------------------------------------
%--------------------------------------------------------------------------------------------------------------
:- begin_tests(civilizacion_alcanzo_una_teconologia).

test("civilizacion alcanzó una tecnologia",nondet) :-
    alcanzo(romanos, herreria).

test("civilizacion no alcanzo una tecnologia", fail) :-
    alcanzo(incas, laminas).

:- end_tests(civilizacion_alcanzo_una_teconologia).

:- begin_tests(civilizacion_lider).

test("es civilizacion lider",nondet) :-
    esLider(romanos).

test("no es civilizacion lider", fail) :-
    esLider(incas).

:- end_tests(civilizacion_lider).

%--------------------------------------------------------------------------------------------------------------
%--------------------------------------------------------------------------------------------------------------

:- begin_tests(unidad_con_mas_vida).

test("unidad con mas vida que tiene una persona",nondet) :-
    conMasVida(ana,jinete(caballo)).

:- end_tests(unidad_con_mas_vida).

%--------------------------------------------------------------------------------------------------------------
%--------------------------------------------------------------------------------------------------------------

:- begin_tests(unaUnidad_gana_a_otraUnidad).

test("la primer unidad gana a la segunda unidad por comparacion de cantidad de vida") :-
    leGana(campeon(95),campeon(50)).

test("la primer unidad no gana a la segunda unidad por ventaja",fail) :-
    leGana(campeon(100),jinete(caballo)).

:- end_tests(unaUnidad_gana_a_otraUnidad).

%--------------------------------------------------------------------------------------------------------------
%--------------------------------------------------------------------------------------------------------------

:- begin_tests(puede_sobrevivr_a_un_asedio).

test("jugador que puede sobrevivir a un asedio",nondet) :-
    puedeSobrevivir(beto).

test("jugador que no puede sobrevir a un asedio") :-
    not(puedeSobrevivir(ana)),not(puedeSobrevivir(carola)),not(puedeSobrevivir(dimitri)).

:- end_tests(puede_sobrevivr_a_un_asedio).

%--------------------------------------------------------------------------------------------------------------
%--------------------------------------------------------------------------------------------------------------

:- begin_tests(jugador_puede_desarrollar_una_tecnologia).

test("jugador que puede desarrollar una tecnologia sin dependencia",nondet) :-
    puedeDesarrollar(beto,molino).

test("jugador no puede desarrolar una tecnologia que ya tiene", fail) :-
    puedeDesarrollar(beto,herreria).

test("jugador no puede desarrolar una tecnologia que ya tiene", nondet) :-
    puedeDesarrollar(ana,fundicion).

:- end_tests(jugador_puede_desarrollar_una_tecnologia).

