% === Declaraciones dinámicas ===
:- dynamic personaje/3.
:- dynamic fruta/4.
:- dynamic miembro_de/2.
:- dynamic amigo/2.
:- dynamic enemigo/2.
:- dynamic familia/2.
:- dynamic organizacion/2.
:- dynamic tiene_rango/2.

:- discontiguous fruta_libre/1.

personaje_fuerte(Personaje) :-
    (   recompensa(Personaje, R) ->                % Si tiene recompensa
        (   R > 1000000000 ->
            format('El personaje ~w es muy fuerte con recompensa de ~w berries.~n', [Personaje, R])
        ;   format('El personaje ~w tiene una recompensa de ~w berries, no es considerado muy fuerte.~n', [Personaje, R])
        )
    ;   format('El personaje ~w no tiene recompensa registrada.~n', [Personaje])
    ).

tiene_fruta(Personaje) :-
    (   personaje(Personaje, Estado, _) ->
        (   fruta(Fruta, _, Personaje) ->
            ( Estado = "vivo" ->
                format('El personaje ~w tiene la fruta del diablo: ~w.~n', [Personaje, Fruta])
            ; Estado = "muerto" ->
                format('El personaje ~w tenía la fruta del diablo: ~w.~n', [Personaje, Fruta])
            )
        ;   ( Estado = "muerto" ->
                format('El personaje ~w no tenía fruta del diablo.~n', [Personaje])
            ; Estado = "vivo" ->
                format('El personaje ~w NO tiene fruta del diablo.~n', [Personaje])
            )
        )
    ;   format('El personaje ~w NO está registrado en la base de datos.~n', [Personaje])
    ).



% fruta ocupada: existe usuario vivo para esa fruta
fruta_ocupada(Fruta) :-
    fruta(Fruta, _, Usuario),
    personaje(Usuario, "vivo", _).

% fruta libre: existe pero no tiene usuario vivo
fruta_libre(Fruta) :-
    fruta(Fruta, _, _),
    \+ fruta_ocupada(Fruta).

% mostrar lista verticalmente
mostrar_lista([]).
mostrar_lista([H|T]) :-
    format('~w~n', [H]),
    mostrar_lista(T).

% mostrar frutas ocupadas
mostrar_frutas_ocupadas :-
    setof(Fruta, fruta_ocupada(Fruta), Lista),
    mostrar_lista(Lista).

% mostrar frutas libres
mostrar_frutas_libres :-
    setof(Fruta, fruta_libre(Fruta), Lista),
    mostrar_lista(Lista).

aliados_de(Personaje) :-
    setof(A, amigo(Personaje, A), Lista), !,
    format('Aliados de ~w:~n', [Personaje]),
    mostrar_lista(Lista).
aliados_de(Personaje) :-
    format('El personaje ~w no tiene aliados registrados.~n', [Personaje]).

% enemigos_de(Personaje).
enemigos_de(Personaje) :-
    setof(E, enemigo(Personaje, E), Lista), !,
    format('Enemigos de ~w:~n', [Personaje]),
    mostrar_lista(Lista).
enemigos_de(Personaje) :-
    format('El personaje ~w no tiene enemigos registrados.~n', [Personaje]).

% familia_de(Personaje).
familia_de(Personaje) :-
    setof(E, familia(Personaje, E), Lista), !,
    format('Familiares de ~w:~n', [Personaje]),
    mostrar_lista(Lista).
familia_de(Personaje) :-
    format('El personaje ~w no tiene familiares registrados.~n', [Personaje]).

miembros_de_organizacion(Org) :-
    setof(Personaje, organizacion(Personaje, Org), Lista), !,
    format('Miembros de la organización "~w":~n', [Org]),
    mostrar_lista(Lista).
miembros_de_organizacion(Org) :-
    format('No se encontraron miembros registrados en la organización "~w".~n', [Org]).


% Listar todos los rangos únicos disponibles
rangos_disponibles(Rangos) :-
    setof(Rango, Persona^rango(Persona, Rango), Rangos).

% Mostrar opciones enumeradas con índice correcto
mostrar_opciones(Lista) :-
    mostrar_opciones(Lista, 1).

mostrar_opciones([], _).
mostrar_opciones([H|T], N) :-
    format('~d. ~w~n', [N, H]),
    N1 is N + 1,
    mostrar_opciones(T, N1).


% Menú interactivo para elegir rango y mostrar personajes
menu_personajes_por_rango :-
    rangos_disponibles(Rangos),
    format('Rangos disponibles:~n'),
    mostrar_opciones(Rangos),
    write('Selecciona el número del rango para ver sus personajes: '),
    read(Opcion),
    nth1(Opcion, Rangos, RangoSeleccionado),
    format('Personajes con rango "~w":~n', [RangoSeleccionado]),
    personajes_por_rango(RangoSeleccionado).

% Mostrar personajes que tienen ese rango
personajes_por_rango(Rango) :-
    setof(Personaje, rango(Personaje, Rango), Lista),
    mostrar_lista(Lista).

% Obtener todos los arcos disponibles
arcos_disponibles(Arcos) :-
    setof(Arco, Personaje^evento(Personaje, Arco), Arcos).

% Menú interactivo para seleccionar arco y mostrar personajes
menu_personajes_por_evento :-
    arcos_disponibles(Arcos),
    format('Arcos disponibles:~n'),
    mostrar_opciones(Arcos),
    write('Selecciona el número del arco para ver sus personajes: '),
    read(Opcion),
    nth1(Opcion, Arcos, ArcoSeleccionado),
    format('Personajes que participaron en "~w":~n', [ArcoSeleccionado]),
    personajes_por_evento(ArcoSeleccionado).

% Mostrar personajes que participaron en un arco
personajes_por_evento(Arco) :-
    setof(Personaje, evento(Personaje, Arco), Lista),
    mostrar_lista(Lista).


% ==== PERSONAJE ====
:- dynamic personaje/3.
:- dynamic fruta/3.
:- dynamic miembro_de/2.
:- dynamic organizacion/2.
:- dynamic rango/2.
:- dynamic recompensa/2.

registrar_personaje_completo :-
    leer_nombre(Personaje),
    elegir_estado(Estado),
    elegir_genero(Genero),
    (personaje(Personaje, Estado, Genero) ->
        format('El personaje ~w ya existe.\n', [Personaje])
    ; assertz(personaje(Personaje, Estado, Genero)),
      guardar_hecho('personaje.pl', personaje(Personaje, Estado, Genero)),
      format('✔ Personaje ~w registrado y guardado.\n', [Personaje]),
      registrar_fruta(Personaje),
      registrar_organizacion(Personaje),
      preguntar_si_pirata(Personaje),
      registrar_recompensa(Personaje)
    ).

preguntar_si_pirata(Personaje) :-
    repeat,
    write('¿El personaje es un pirata? (s/n): '), read(Rta),
    (Rta = s ->
        registrar_tripulacion(Personaje)
    ; Rta = n -> true
    ; writeln('Respuesta inválida.'), fail).


leer_nombre(N) :-
    write('Nombre del personaje (entre comillas dobles): '), read(N).

elegir_estado(E) :-
    repeat,
    writeln('Selecciona estado:'),
    writeln('1. vivo'),
    writeln('2. muerto'),
    write('Opción: '), read(Opc),
    (Opc = 1 -> E = "vivo"; Opc = 2 -> E = "muerto"; writeln('Opción inválida.'), fail).

elegir_genero(G) :-
    repeat,
    writeln('Selecciona género:'),
    writeln('1. masculino'),
    writeln('2. femenino'),
    write('Opción: '), read(Opc),
    (Opc = 1 -> G = "masculino"; Opc = 2 -> G = "femenino"; writeln('Opción inválida.'), fail).

registrar_fruta(Personaje) :-
    repeat,
    write('¿El personaje tiene fruta del diablo? (s/n): '), read(Rta),
    (Rta = s -> manejar_fruta(Personaje);
     Rta = n -> true;
     writeln('Respuesta inválida.'), fail).

manejar_fruta(Personaje) :-
    write('Nombre de la fruta (entre comillas dobles): '), read(F),
    ( fruta_ocupada(F) ->
        format('La fruta ~w ya está ocupada por un usuario vivo.\n', [F]),
        write('¿Quieres registrar otra fruta? (s/n): '), read(R),
        (R = s -> manejar_fruta(Personaje); R = n -> true; writeln('Respuesta inválida.'), manejar_fruta(Personaje))
    ;   elegir_tipo_fruta(Tipo),
        assertz(fruta(F, Tipo, Personaje)),
        guardar_hecho('fruta.pl', fruta(F, Tipo, Personaje)),
        format('✔ Fruta ~w de tipo ~w agregada para ~w.\n', [F, Tipo, Personaje])
    ).

elegir_tipo_fruta(T) :-
    repeat,
    writeln('Selecciona tipo de fruta:'),
    writeln('1. logia'),
    writeln('2. zoan'),
    writeln('3. paramecia'),
    writeln('4. mitologica'),
    write('Opción: '), read(Opc),
    (Opc = 1 -> T = "logia";
     Opc = 2 -> T = "zoan";
     Opc = 3 -> T = "paramecia";
     Opc = 4 -> T = "mitologica";
     writeln('Opción inválida.'), fail).

registrar_organizacion(Personaje) :-
    repeat,
    write('¿El personaje pertenece a una organización? (s/n): '), read(Rta),
    (   Rta = s ->
        elegir_organizacion(Personaje),
        !
    ;   Rta = n ->
        !
    ;   writeln('Respuesta inválida, por favor ingresa s o n.'),
        fail
    ).

elegir_organizacion(Personaje) :-
    obtener_organizaciones(OrgListBase),
    append(OrgListBase, ["otro"], OrgList),
    mostrar_opciones(OrgList),
    write('Selecciona organización (número): '), read(Opc),
    length(OrgList, Len),
    (   integer(Opc), Opc >= 1, Opc =< Len ->
        nth1(Opc, OrgList, Org),
        (   Org = "otro" ->
            write('Ingrese el nombre de la organización (entre comillas dobles): '), read(NuevaOrg),
            assertz(organizacion(Personaje, NuevaOrg)),
            guardar_hecho('organizaciones.pl', organizacion(Personaje, NuevaOrg)),
            format('✔ Organización ~w agregada y asignada a ~w.~n', [NuevaOrg, Personaje])
        ;   assertz(organizacion(Personaje, Org)),
            guardar_hecho('organizaciones.pl', organizacion(Personaje, Org)),
            (Org = "marina" ->
                registrar_rango(Personaje)
            ; Org = "pirata" ->
                registrar_tripulacion(Personaje)
            ; true
            ),
            format('✔ ~w asignado a la organización ~w.~n', [Personaje, Org])
        )
    ;   writeln('Opción inválida.'), elegir_organizacion(Personaje)
    ).


% Obtener lista base de organizaciones
obtener_organizaciones(OrgList) :-
    setof(O, P^organizacion(P, O), OrgList), !.
obtener_organizaciones(["marina", "pirata", "shichibukai", "yonkou", "revolucionarios"]). % default


registrar_rango(Personaje) :-
    obtener_rangos(Rangos),
    mostrar_opciones(Rangos),
    write('Selecciona rango (número): '), read(Opc),
    length(Rangos, Len),
    (integer(Opc), Opc >= 1, Opc =< Len ->
        nth1(Opc, Rangos, Rango),
        assertz(rango(Personaje, Rango)),
        guardar_hecho('rango.pl', rango(Personaje, Rango)),
        format('✔ Rango ~w agregado para ~w.\n', [Rango, Personaje])
    ; writeln('Opción inválida.'), registrar_rango(Personaje)).

obtener_rangos(Rangos) :-
    setof(R, P^rango(P, R), Rangos), !.
obtener_rangos(["capitan", "vicealmirante", "almirante", "almirante jefe", "almirante de flota"]). % Default si no hay

registrar_tripulacion(Personaje) :-
    obtener_tripulaciones(TripListBase),
    append(TripListBase, ["otro"], TripList),
    mostrar_opciones(TripList),
    write('Selecciona tripulación (número): '), read(Opc),
    length(TripList, Len),
    (integer(Opc), Opc >= 1, Opc =< Len ->
        nth1(Opc, TripList, Trip),
        (Trip = "otro" ->
            write('Ingrese el nombre de la tripulación (entre comillas dobles): '), read(NuevaTrip),
            assertz(miembro_de(Personaje, NuevaTrip)),
            guardar_hecho('tripulacion.pl', miembro_de(Personaje, NuevaTrip)),
            format('✔ Tripulación ~w agregada y asignada a ~w.~n', [NuevaTrip, Personaje])
        ; assertz(miembro_de(Personaje, Trip)),
          guardar_hecho('tripulacion.pl', miembro_de(Personaje, Trip)),
          format('✔ ~w asignado a la tripulación ~w.~n', [Personaje, Trip])
        )
    ; writeln('Opción inválida.'), registrar_tripulacion(Personaje)
    ).

% Obtener lista base de tripulaciones
obtener_tripulaciones(TripList) :-
    setof(T, P^miembro_de(P, T), TripList), !.
obtener_tripulaciones(["Mugiwara", "Piratas Barba Blanca", "Piratas Barbanegra", "Bestias", "Piratas Big Mom"]). % default

registrar_recompensa(Personaje) :-
    repeat,
    write('¿El personaje tiene recompensa? (s/n): '), read(Rta),
    (Rta = s ->
        write('Ingrese el valor de la recompensa (número entero): '), read(V),
        assertz(recompensa(Personaje, V)),
        guardar_hecho('recompensas.pl', recompensa(Personaje, V)),
        format('✔ Recompensa de ~w agregada y guardada: ~w berries.\n', [Personaje, V])
    ; Rta = n -> true
    ; writeln('Respuesta inválida.'), fail).

% Guardar el hecho en archivo con comillas automáticas
guardar_hecho(Archivo, Hecho) :-
    open(Archivo, append, Stream),
    writeq(Stream, Hecho),
    write(Stream, '.'),
    nl(Stream),        % Salto de línea después del punto
    close(Stream).

% Mostrar opciones enumeradas con índice
mostrar_opciones(Lista) :-
    mostrar_opciones(Lista, 1).

mostrar_opciones([], _).
mostrar_opciones([H|T], N) :-
    format('~d. ~w~n', [N, H]),
    N1 is N + 1,
    mostrar_opciones(T, N1).

% ==== Eliminación completa de un personaje y reescritura de archivos ====

eliminar_personaje(Personaje) :-
    retractall(personaje(Personaje, _, _)),
    retractall(fruta(_, _, Personaje)),
    retractall(organizacion(Personaje, _)),
    retractall(rango(Personaje, _)),
    retractall(miembro_de(Personaje, _)),
    retractall(recompensa(Personaje, _)),
    retractall(amigo(Personaje, _)),
    retractall(amigo(_, Personaje)),
    retractall(enemigo(Personaje, _)),
    retractall(enemigo(_, Personaje)),
    retractall(familia(Personaje, _)),
    retractall(familia(_, Personaje)),
    retractall(evento(Personaje, _)),
    guardar_todos_los_archivos.

% ==== Guardar todos los hechos en archivos ====

guardar_todos_los_archivos :-
    guardar_hechos(personaje, 3, 'personaje.pl'),
    guardar_hechos(fruta, 3, 'fruta.pl'),
    guardar_hechos(organizacion, 2, 'organizaciones.pl'),
    guardar_hechos(rango, 2, 'rango.pl'),
    guardar_hechos(miembro_de, 2, 'tripulacion.pl'),
    guardar_hechos(recompensa, 2, 'recompensas.pl'),
    guardar_hechos_multiples([amigo, enemigo, familia], 2, 'relaciones.pl'),
    guardar_hechos(evento, 2, 'eventos.pl').


guardar_hechos_multiples(Preds, Aridad, Archivo) :-
    open(Archivo, write, Stream),
    guardar_predicados_en_stream(Preds, Aridad, Stream),
    close(Stream).

guardar_predicados_en_stream([], _, _).
guardar_predicados_en_stream([Pred|T], Aridad, Stream) :-
    functor(Termino, Pred, Aridad),
    forall(clause(Termino, true),
           (writeq(Stream, Termino), write(Stream, '.'), nl(Stream))),
    guardar_predicados_en_stream(T, Aridad, Stream).

% ==== Guardar hechos de cualquier aridad ====

guardar_hechos(Predicado, Aridad, Archivo) :-
    functor(Termino, Predicado, Aridad),
    open(Archivo, write, Stream),
    forall(clause(Termino, true),
           (writeq(Stream, Termino), write(Stream, '.'), nl(Stream))),
    close(Stream).






