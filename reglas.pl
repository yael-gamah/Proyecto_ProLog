% === Declaraciones dinámicas ===
:- dynamic personaje/3.
:- dynamic fruta/4.
:- dynamic miembro_de/2.
:- dynamic amigo/2.
:- dynamic enemigo/2.
:- dynamic familia/2.
:- dynamic esta_en/2.
:- dynamic tiene_rango/2.


% tiene_rango(Personaje, Rango).
tiene_rango(garp, vicealmirante).
tiene_rango(sengoku, almirante).
tiene_rango(akainu, almirante_jefe).
tiene_rango(kizaru, almirante).
tiene_rango(aokiji, almirante).
tiene_rango(smoker, vicealmirante).
tiene_rango(koby, capitan).

% personaje_fuerte(X) :- recompensa(X, R), R > 1000000000.
personaje_fuerte(X) :- recompensa(X, R), R > 1000000000.

% rival(X, Y) :- enemigo(X, Y), enemigo(Y, X).
rival(X, Y) :- enemigo(X, Y), enemigo(Y, X).

% sin_fruta(X) :- personaje(X, _, _), \+ fruta(_, _, X, _).
sin_fruta(X) :- personaje(X, _, _), \+ fruta(_, _, X, _).

% fruta_ocupada(NombreFruta) :- tiene dueño actualmente.
fruta_ocupada(Fruta) :- fruta(Fruta, _, Usuario, ocupada), personaje(Usuario, _, _).

% fruta_libre(NombreFruta).
fruta_libre(Fruta) :- fruta(Fruta, _, _, libre).

% tipo_fruta_de(Personaje, Tipo).
tipo_fruta_de(Personaje, Tipo) :- fruta(_, Tipo, Personaje, ocupada).

% personajes_con_fruta_tipo(Tipo, Lista).
personajes_con_fruta_tipo(Tipo, Lista) :-
    setof(Personaje, fruta(_, Tipo, Personaje, ocupada), Lista).

% enemigos_mutuos(X, Y) :- ambos se tienen como enemigos.
enemigos_mutuos(X, Y) :- enemigo(X, Y), enemigo(Y, X).

% enemigos_de_tripulacion(Tripulacion, Enemigos).
enemigos_de_tripulacion(Trip, Lista) :-
    setof(Enemigo, Miembro^(miembro_de(Miembro, Trip), enemigo(Miembro, Enemigo)), Lista).

% aliados_de(Personaje, Lista).
aliados_de(P, Lista) :-
    setof(A, amigo(P, A), Lista), !.
aliados_de(_, []).

% enemigos_de(Personaje, Lista).
enemigos_de(P, Lista) :-
    setof(E, enemigo(P, E), Lista), !.
enemigos_de(_, []).

% miembros_de_organizacion(Org, Lista).
miembros_de_organizacion(Org, Lista) :-
    setof(Persona, esta_en(Persona, Org), Lista).

% personajes_con_rango(Rango, Lista).
personajes_con_rango(Rango, Lista) :-
    setof(P, tiene_rango(P, Rango), Lista).

% visito_mas_de(Personaje, N) :- visitó más de N islas.
visito_mas_de(Personaje, N) :-
    setof(Isla, visito(Personaje, Isla), Lista),
    length(Lista, Cant), Cant > N.

% ha_estado_en_misma_isla(X, Y) :- coincidieron en al menos una isla.
ha_estado_en_misma_isla(X, Y) :-
    visito(X, Isla), visito(Y, Isla), X \= Y.

% personajes_en_evento(Evento, Lista).
personajes_en_evento(Ev, Lista) :-
    setof(P, participo_en(P, Ev), Lista).

% personajes_en_multiples_eventos(Personaje, ListaEventos).
personajes_en_multiples_eventos(P, Lista) :-
    setof(E, participo_en(P, E), Lista), length(Lista, L), L > 1.

% personaje_muy_peligroso(P) :- recompensa alta + fruta logia o mitológica.
personaje_muy_peligroso(P) :-
    recompensa(P, R), R > 1000000000,
    (fruta(_, logia, P, ocupada); fruta(_, mitologica, P, ocupada)).

% puede_derrotar(P1, P2) :- recompensa de P1 es mayor y P1 tiene fruta.
puede_derrotar(P1, P2) :-
    recompensa(P1, R1), recompensa(P2, R2), R1 > R2,
    fruta(_, _, P1, ocupada).

% sin_tripulacion(Personaje).
sin_tripulacion(P) :- personaje(P, _, _), \+ miembro_de(P, _).

% sin_organizacion(Personaje).
sin_organizacion(P) :- personaje(P, _, _), \+ esta_en(P, _).

% personajes_muertos(Lista).
personajes_muertos(Lista) :-
    setof(P, personaje(P, muerto, _), Lista).

% personajes_con_fruta(Lista).
personajes_con_fruta(Lista) :-
    setof(P, fruta(_, _, P, ocupada), Lista).

% usuario_actual_de(Fruta, Usuario): devuelve el usuario si está ocupada.
usuario_actual_de(F, U) :- fruta(F, _, U, ocupada).

% fruta_heredada(F): la fruta la tuvo alguien que murió y ahora la tiene otro.
fruta_heredada(F) :-
    fruta(F, _, Antiguo, ocupada),
    personaje(Antiguo, muerto, _),
    fruta(F, _, Nuevo, ocupada),
    personaje(Nuevo, vivo, _),
    Antiguo \= Nuevo.

% recompensa_mayor(P1, P2): P1 tiene más recompensa que P2.
recompensa_mayor(P1, P2) :-
    recompensa(P1, R1),
    recompensa(P2, R2),
    R1 > R2.

yonkou_actuales(L) :-
    setof(P, (esta_en(P, yonkou), personaje(P, vivo, _)), L).

familiares_de(P, Lista) :-
    setof(F, familia(P, F), Lista), !.
familiares_de(_, []).

% ==== PERSONAJE ====
registrar_personaje :-
    write('Nombre del personaje: '), read(N),
    write('Estado (vivo/muerto): '), read(E),
    write('Genero (masculino/femenino): '), read(G),
    ( personaje(N, E, G) ->
        format('El personaje ~w ya existe.~n', [N])
    ; assertz(personaje(N, E, G)),
      append('C:/Users/YaelG/OneDrive/Documentos/ESCUELA/SEMESTRE 8/ProLog/Proyecto_ProLog/personaje_guardado.pl'),
      write(personaje(N, E, G)), write('.'), nl, nl,
      told,
      format('✔ Personaje ~w registrado y guardado en archivo.~n', [N])
    ).



eliminar_personaje :-
    write('Nombre: '), read(N),
    write('Estado: '), read(E),
    write('Genero: '), read(G),
    retract(personaje(N, E, G)),
    write('❌ Personaje eliminado: '), write(N), nl.

% ==== FRUTA DEL DIABLO ====
insertar_fruta :-
    write('Nombre de la fruta: '), read(N),
    write('Tipo (logia/zoan/paramecia/mitologica): '), read(T),
    write('Usuario actual (o nadie): '), read(U),
    write('Estado (ocupada/libre): '), read(E),
    assertz(fruta(N, T, U, E)),
    write('✔ Fruta insertada: '), write(N), nl.

eliminar_fruta :-
    write('Nombre: '), read(N),
    write('Tipo: '), read(T),
    write('Usuario: '), read(U),
    write('Estado: '), read(E),
    retract(fruta(N, T, U, E)),
    write('❌ Fruta eliminada: '), write(N), nl.

% ==== TRIPULACIÓN ====
insertar_miembro :-
    write('Personaje: '), read(P),
    write('Tripulacion: '), read(T),
    assertz(miembro_de(P, T)),
    write('✔ Miembro agregado a la tripulación: '), write(T), nl.

eliminar_miembro :-
    write('Personaje: '), read(P),
    write('Tripulacion: '), read(T),
    retract(miembro_de(P, T)),
    write('❌ Miembro eliminado de la tripulación: '), write(T), nl.

% ==== RELACIONES ====
insertar_amigo :-
    write('Personaje 1: '), read(P1),
    write('Personaje 2: '), read(P2),
    assertz(amigo(P1, P2)),
    write('✔ Amistad registrada entre: '), write(P1), write(' y '), write(P2), nl.

eliminar_amigo :-
    write('Personaje 1: '), read(P1),
    write('Personaje 2: '), read(P2),
    retract(amigo(P1, P2)),
    write('❌ Amistad eliminada entre: '), write(P1), write(' y '), write(P2), nl.

insertar_enemigo :-
    write('Personaje 1: '), read(P1),
    write('Personaje 2: '), read(P2),
    assertz(enemigo(P1, P2)),
    write('✔ Enemistad registrada entre: '), write(P1), write(' y '), write(P2), nl.

insertar_familia :-
    write('Familiar 1: '), read(P1),
    write('Familiar 2: '), read(P2),
    assertz(familia(P1, P2)),
    write('✔ Relación familiar registrada entre: '), write(P1), write(' y '), write(P2), nl.

eliminar_familia :-
    write('Familiar 1: '), read(P1),
    write('Familiar 2: '), read(P2),
    retract(familia(P1, P2)),
    write('❌ Relación familiar eliminada entre: '), write(P1), write(' y '), write(P2), nl.
