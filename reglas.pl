% === Declaraciones dinámicas ===
:- dynamic personaje/3.
:- dynamic fruta/3.
:- dynamic miembro_de/2.
:- dynamic organizacion/2.
:- dynamic rango/2.
:- dynamic recompensa/2.
:- dynamic amigo/2.
:- dynamic enemigo/2.
:- dynamic familia/2.
:- dynamic evento/2.

% === Utilidades ===

mostrar_lista([]).
mostrar_lista([H|T]) :-
    format('~w~n', [H]),
    mostrar_lista(T).

mostrar_opciones(Lista) :-
    mostrar_opciones(Lista, 1).
mostrar_opciones([], _).
mostrar_opciones([H|T], N) :-
    format('~d. ~w~n', [N, H]),
    N1 is N + 1,
    mostrar_opciones(T, N1).

guardar_hecho(Archivo, Hecho) :-
    open(Archivo, append, Stream),
    writeq(Stream, Hecho), write(Stream, '.'), nl(Stream),
    close(Stream).

guardar_hechos(Hecho, Aridad, Archivo) :-
    functor(Term, Hecho, Aridad),
    open(Archivo, write, Stream),
    forall(clause(Term, true),
           (writeq(Stream, Term), write(Stream, '.'), nl(Stream))),
    close(Stream).

guardar_hechos_multiples(Preds, Aridad, Archivo) :-
    open(Archivo, write, Stream),
    guardar_hechos_en_stream(Preds, Aridad, Stream),
    close(Stream).

guardar_hechos_en_stream([], _, _).
guardar_hechos_en_stream([Pred|T], Aridad, Stream) :-
    functor(Term, Pred, Aridad),
    forall(clause(Term, true),
           (writeq(Stream, Term), write(Stream, '.'), nl(Stream))),
    guardar_hechos_en_stream(T, Aridad, Stream).

guardar_todos_los_archivos :-
    guardar_hechos(personaje, 3, 'personaje.pl'),
    guardar_hechos(fruta, 3, 'fruta.pl'),
    guardar_hechos(organizacion, 2, 'organizaciones.pl'),
    guardar_hechos(rango, 2, 'rango.pl'),
    guardar_hechos(miembro_de, 2, 'tripulacion.pl'),
    guardar_hechos(recompensa, 2, 'recompensas.pl'),
    guardar_hechos_multiples([amigo, enemigo, familia], 2, 'relaciones.pl'),
    guardar_hechos(evento, 2, 'eventos.pl').

% === Consultas de personajes ===

personaje_fuerte(P) :-
    (recompensa(P, R) ->
        (R > 1000000000 ->
            format('~w es muy fuerte con recompensa de ~w berries.~n', [P, R])
        ;   format('~w tiene recompensa de ~w berries, no es tan fuerte.~n', [P, R]))
    ;   format('~w no tiene recompensa registrada.~n', [P])).

tiene_fruta(P) :-
    (personaje(P, Estado, _),
     (fruta(F, _, P) ->
        (Estado = "vivo" -> format('~w tiene la fruta: ~w.~n', [P, F])
        ; format('~w tenía la fruta: ~w.~n', [P, F]))
     ;  format('~w NO tiene fruta del diablo.~n', [P]))
    ; format('~w no está registrado.~n', [P])).

fruta_ocupada(F) :- fruta(F, _, P), personaje(P, "vivo", _).
fruta_libre(F) :- fruta(F, _, _), \+ fruta_ocupada(F).

mostrar_frutas_ocupadas :- setof(F, fruta_ocupada(F), L), mostrar_lista(L).
mostrar_frutas_libres :- setof(F, fruta_libre(F), L), mostrar_lista(L).

aliados_de(P) :- setof(A, amigo(P, A), L), !,
    format('Aliados de ~w:~n', [P]), mostrar_lista(L).
aliados_de(P) :- format('~w no tiene aliados registrados.~n', [P]).

enemigos_de(P) :- setof(E, enemigo(P, E), L), !,
    format('Enemigos de ~w:~n', [P]), mostrar_lista(L).
enemigos_de(P) :- format('~w no tiene enemigos registrados.~n', [P]).

familia_de(P) :- setof(F, familia(P, F), L), !,
    format('Familiares de ~w:~n', [P]), mostrar_lista(L).
familia_de(P) :- format('~w no tiene familiares registrados.~n', [P]).

miembros_de_organizacion(O) :-
    setof(P, organizacion(P, O), L), !,
    format('Miembros de la organización "~w":~n', [O]), mostrar_lista(L).
miembros_de_organizacion(O) :-
    format('No se encontraron miembros en la organización "~w".~n', [O]).

% === Menús Interactivos ===

rangos_disponibles(R) :- setof(Rango, P^rango(P, Rango), R).
arcos_disponibles(A) :- setof(Arco, P^evento(P, Arco), A).

menu_personajes_por_rango :-
    rangos_disponibles(Rangos),
    writeln('Rangos disponibles:'), mostrar_opciones(Rangos),
    write('Selecciona número: '), read(Opc),
    nth1(Opc, Rangos, R),
    format('Personajes con rango "~w":~n', [R]),
    setof(P, rango(P, R), L), mostrar_lista(L).

menu_personajes_por_evento :-
    arcos_disponibles(Arcos),
    writeln('Arcos disponibles:'), mostrar_opciones(Arcos),
    write('Selecciona número: '), read(Opc),
    nth1(Opc, Arcos, A),
    format('Participantes del arco "~w":~n', [A]),
    setof(P, evento(P, A), L), mostrar_lista(L).

% === Registro completo de un personaje ===

registrar_personaje_completo :-
    leer_nombre(Personaje),
    elegir_estado(Estado),
    elegir_genero(Genero),
    (personaje(Personaje, _, _) ->
        format('El personaje ~w ya está registrado.~n', [Personaje]), !
    ; assertz(personaje(Personaje, Estado, Genero)),
      guardar_hecho('personaje.pl', personaje(Personaje, Estado, Genero)),
      format('Personaje ~w registrado.~n', [Personaje]),
      preguntar_fruta(Personaje),
      preguntar_organizacion(Personaje),
      preguntar_si_pirata(Personaje),
      preguntar_recompensa(Personaje), !
    ).

leer_nombre(Nombre) :-
    write('Nombre del personaje (entre comillas dobles): '),
    read(Nombre).

elegir_estado(Estado) :-
    repeat,
    writeln('Selecciona estado:'),
    writeln('1. vivo'),
    writeln('2. muerto'),
    write('Opción: '), read(Opc),
    (Opc = 1 -> Estado = "vivo";
     Opc = 2 -> Estado = "muerto";
     writeln('Opción inválida.'), fail).

elegir_genero(Genero) :-
    repeat,
    writeln('Selecciona género:'),
    writeln('1. masculino'),
    writeln('2. femenino'),
    write('Opción: '), read(Opc),
    (Opc = 1 -> Genero = "masculino";
     Opc = 2 -> Genero = "femenino";
     writeln('Opción inválida.'), fail).

% === Fruta del diablo ===

preguntar_fruta(Personaje) :-
    write('¿Tiene fruta del diablo? (s/n): '), read(R),
    (R = s -> registrar_fruta(Personaje); true).

registrar_fruta(Personaje) :-
    repeat,
    write('Nombre de la fruta (entre comillas dobles): '), read(Fruta),
    (fruta_ocupada(Fruta) ->
        format('La fruta ~w ya está ocupada.~n', [Fruta]),
        write('¿Deseas registrar otra? (s/n): '), read(Resp),
        (Resp = s -> fail; true)
    ; elegir_tipo_fruta(Tipo),
      assertz(fruta(Fruta, Tipo, Personaje)),
      guardar_hecho('fruta.pl', fruta(Fruta, Tipo, Personaje)),
      format('Fruta ~w registrada para ~w.~n', [Fruta, Personaje])
    ).

elegir_tipo_fruta(Tipo) :-
    repeat,
    writeln('Tipo de fruta:'),
    writeln('1. logia'), writeln('2. zoan'), writeln('3. paramecia'), writeln('4. mitologica'),
    write('Opción: '), read(Opc),
    (Opc = 1 -> Tipo = "logia";
     Opc = 2 -> Tipo = "zoan";
     Opc = 3 -> Tipo = "paramecia";
     Opc = 4 -> Tipo = "mitologica";
     writeln('Opción inválida.'), fail).

% === Organización y Rango ===

preguntar_organizacion(Personaje) :-
    write('¿Pertenece a una organización? (s/n): '), read(R),
    (R = s -> seleccionar_organizacion(Personaje); true).

seleccionar_organizacion(Personaje) :-
    obtener_organizaciones(Base),
    append(Base, ["otro"], Lista),
    mostrar_opciones(Lista),
    write('Selecciona organización (número): '), read(Opc),
    nth1(Opc, Lista, Org),
    (Org = "otro" ->
        write('Nombre de nueva organización: '), read(Nueva),
        OrgFinal = Nueva
    ; OrgFinal = Org),
    assertz(organizacion(Personaje, OrgFinal)),
    guardar_hecho('organizaciones.pl', organizacion(Personaje, OrgFinal)),
    format('Organización ~w asignada a ~w.~n', [OrgFinal, Personaje]),
    (OrgFinal = "marina" -> seleccionar_rango(Personaje); true).

obtener_organizaciones(Lista) :-
    setof(O, P^organizacion(P, O), Lista), !.
obtener_organizaciones(["marina", "pirata", "shichibukai", "yonkou", "revolucionarios"]).

seleccionar_rango(Personaje) :-
    obtener_rangos(Base),
    mostrar_opciones(Base),
    write('Selecciona rango (número): '), read(Opc),
    nth1(Opc, Base, Rango),
    assertz(rango(Personaje, Rango)),
    guardar_hecho('rango.pl', rango(Personaje, Rango)),
    format('Rango ~w asignado a ~w.~n', [Rango, Personaje]).

obtener_rangos(Rangos) :-
    setof(R, P^rango(P, R), Rangos), !.
obtener_rangos(["capitan", "vicealmirante", "almirante", "almirante jefe", "almirante de flota"]).

% === Tripulación ===

preguntar_si_pirata(Personaje) :-
    write('¿Es un pirata? (s/n): '), read(R),
    (R = s -> seleccionar_tripulacion(Personaje); true).

seleccionar_tripulacion(Personaje) :-
    obtener_tripulaciones(Base),
    append(Base, ["otro"], Lista),
    mostrar_opciones(Lista),
    write('Selecciona tripulación (número): '), read(Opc),
    nth1(Opc, Lista, Trip),
    (Trip = "otro" ->
        write('Nombre de nueva tripulación: '), read(Nueva),
        TripFinal = Nueva
    ; TripFinal = Trip),
    assertz(miembro_de(Personaje, TripFinal)),
    guardar_hecho('tripulacion.pl', miembro_de(Personaje, TripFinal)),
    format('Tripulación ~w asignada a ~w.~n', [TripFinal, Personaje]).

obtener_tripulaciones(Lista) :-
    setof(T, P^miembro_de(P, T), Lista), !.
obtener_tripulaciones(["Mugiwara", "Piratas Barba Blanca", "Piratas Barbanegra", "Bestias", "Piratas Big Mom"]).

% === Recompensa ===

preguntar_recompensa(Personaje) :-
    write('¿Tiene recompensa? (s/n): '), read(R),
    (R = s ->
        write('Monto de recompensa (número entero): '), read(V),
        assertz(recompensa(Personaje, V)),
        guardar_hecho('recompensas.pl', recompensa(Personaje, V)),
        format('✔ Recompensa registrada: ~w berries.~n', [V])
    ; true).


% === Eliminación de personaje ===

eliminar_personaje(P) :-
    retractall(personaje(P, _, _)),
    retractall(fruta(_, _, P)),
    retractall(organizacion(P, _)),
    retractall(rango(P, _)),
    retractall(miembro_de(P, _)),
    retractall(recompensa(P, _)),
    retractall(amigo(P, _)), retractall(amigo(_, P)),
    retractall(enemigo(P, _)), retractall(enemigo(_, P)),
    retractall(familia(P, _)), retractall(familia(_, P)),
    retractall(evento(P, _)),
    guardar_todos_los_archivos.

% === Menú principal del sistema ===

menu :-
    repeat,
    nl, writeln('==== SISTEMA EXPERTO DE ONE PIECE ===='), nl,
    writeln('1. Registrar nuevo personaje'),
    writeln('2. Mostrar frutas ocupadas'),
    writeln('3. Mostrar frutas libres'),
    writeln('4. Ver aliados de un personaje'),
    writeln('5. Ver enemigos de un personaje'),
    writeln('6. Ver familiares de un personaje'),
    writeln('7. Ver miembros de una organización'),
    writeln('8. Ver personajes por rango'),
    writeln('9. Ver personajes por arco'),
    writeln('10. Relacionar personajes'),
    writeln('11. Eliminar personaje'),
    writeln('0. Salir'),
    nl,
    write('Seleccione una opción: '), read(Opcion),
    ejecutar_opcion(Opcion),
    Opcion = 0, !.

% === Ejecutar opción seleccionada ===

ejecutar_opcion(1) :- registrar_personaje_completo.
ejecutar_opcion(2) :- mostrar_frutas_ocupadas.
ejecutar_opcion(3) :- mostrar_frutas_libres.
ejecutar_opcion(4) :- pedir_personaje(aliados_de).
ejecutar_opcion(5) :- pedir_personaje(enemigos_de).
ejecutar_opcion(6) :- pedir_personaje(familia_de).
ejecutar_opcion(7) :- 
    write('Nombre de la organización: '), read(Org),
    miembros_de_organizacion(Org).
ejecutar_opcion(8) :- menu_personajes_por_rango.
ejecutar_opcion(9) :- menu_personajes_por_evento.
ejecutar_opcion(10) :- relacionar_personajes.
ejecutar_opcion(11) :- 
    write('Nombre del personaje a eliminar: '), read(P),
    eliminar_personaje(P),
    format('Personaje ~w eliminado de todos los registros.~n', [P]).
ejecutar_opcion(0) :- writeln('Saliendo del sistema...').
ejecutar_opcion(_) :- writeln('Opción no válida, intente de nuevo.'), fail.

% === Auxiliar para preguntar por personaje ===

pedir_personaje(Regla) :-
    write('Nombre del personaje: '), read(Personaje),
    call(Regla, Personaje).

% === Relacionar personajes ===

relacionar_personajes :-
    obtener_personajes(Lista),
    mostrar_opciones(Lista),
    write('Selecciona el número del primer personaje: '), read(Idx1),
    nth1(Idx1, Lista, P1),

    excluir_elemento(P1, Lista, Lista2),
    mostrar_opciones(Lista2),
    write('Selecciona el número del segundo personaje: '), read(Idx2),
    nth1(Idx2, Lista2, P2),

    writeln('Selecciona el tipo de relación:'),
    writeln('1. Amigo'),
    writeln('2. Enemigo'),
    writeln('3. Familiar'),
    write('Opción: '), read(Op),
    asignar_relacion(Op, P1, P2),
    format('Relación registrada entre ~w y ~w.~n', [P1, P2]).

% Obtener todos los personajes únicos
obtener_personajes(Lista) :-
    setof(N, E^G^personaje(N, E, G), Lista).

% Excluir un elemento de la lista
excluir_elemento(X, Lista, SinX) :- delete(Lista, X, SinX).

% Asignar la relación
asignar_relacion(1, A, B) :-
    assertz(amigo(A, B)),
    guardar_hecho('relaciones.pl', amigo(A, B)).
asignar_relacion(2, A, B) :-
    assertz(enemigo(A, B)),
    guardar_hecho('relaciones.pl', enemigo(A, B)).
asignar_relacion(3, A, B) :-
    assertz(familia(A, B)),
    guardar_hecho('relaciones.pl', familia(A, B)).
asignar_relacion(_, _, _) :-
    writeln('Opción no válida. Relación no registrada.'), fail.
