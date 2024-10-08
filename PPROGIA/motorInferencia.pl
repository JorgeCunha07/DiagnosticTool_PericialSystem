% Versão preparada para lidar com regras que contenham negação (nao)
% Metaconhecimento
% Usar base de conhecimento veIculos2.txt
% Explicações como?(how?) e porque não?(whynot?)

:- op(220, xfx, entao).
:- op(35, xfy, se).
:- op(240, fx, regra).
:- op(500, fy, nao).
:- op(600, xfy, e).

:- dynamic justifica/3.
:- dynamic regra_disparadas/2.
:- dynamic facto/2.
:- dynamic ultimo_facto/1.
:- dynamic factos_processados/1.

% Carregar a base de conhecimento
carrega_bc :-
    write('NOME DA BASE DE CONHECIMENTO (terminar com .)-> '),
    read(NBC),
    consult(NBC).

% Iniciar o motor de inferência
% arranca_motor:- facto(N,Facto),
%		facto_dispara_regras1(Facto, LRegras),
%		dispara_regras(N, Facto, LRegras),
%		ultimo_facto(N).

arranca_motor :-
    retractall(regra_disparadas(_, _)),
    arranca_motor_loop.

% Loop para processar novos factos
arranca_motor_loop :-
    facto(N, Facto),
    \+ factos_processados(N),
    asserta(factos_processados(N)),
    facto_dispara_regras1(Facto, LRegras),
    dispara_regras(N, Facto, LRegras),
    fail.
arranca_motor_loop.

% Verificar se o facto dispara regras
facto_dispara_regras1(Facto, LRegras) :-
    facto_dispara_regras(Facto, LRegras),
    !.
% Caso em que o facto não origina o disparo de qualquer regra.
facto_dispara_regras1(_, []).

% Disparar regras baseadas no facto
dispara_regras(N, Facto, [ID | LRegras]) :-
    regra ID se LHS entao RHS,
    facto_esta_numa_condicao(Facto, LHS),
    verifica_condicoes(LHS, LFactos),
    member(N, LFactos),
    sort(LFactos, SortedLFactos),
    (   regra_disparadas(ID, SortedLFactos) ->
        % Regra já foi disparada com estas condições; ignora
        true
    ;   
        % Dispara a regra e registra que foi disparada com estas condições
        concluir(RHS, ID, LFactos),
        asserta(regra_disparadas(ID, SortedLFactos))
    ),
    !,
    dispara_regras(N, Facto, LRegras).

dispara_regras(N, Facto, [ID | LRegras]) :-
    regra ID se LHS entao RHS e RHS2,
    facto_esta_numa_condicao(Facto, LHS),
    verifica_condicoes(LHS, LFactos),
    member(N, LFactos),
    sort(LFactos, SortedLFactos),
    (   regra_disparadas(ID, SortedLFactos) ->
        % Regra já foi disparada com estas condições; ignora
        true
    ;   
        % Dispara a regra e registra que foi disparada com estas condições
        concluir(RHS, ID, LFactos),
        concluir(RHS2, ID, LFactos),
        asserta(regra_disparadas(ID, SortedLFactos))
    ),
    !,
    dispara_regras(N, Facto, LRegras).

dispara_regras(N, Facto, [_ | LRegras]) :-
    dispara_regras(N, Facto, LRegras).

dispara_regras(_, _, []).

% Verificar se o facto está numa condição
facto_esta_numa_condicao(F, [F e _]).
facto_esta_numa_condicao(F, [avalia(F1) e _]) :-
    F =.. [H, H1 | _],
    F1 =.. [H, H1 | _].
facto_esta_numa_condicao(F, [_ e Fs]) :-
    facto_esta_numa_condicao(F, [Fs]).
facto_esta_numa_condicao(F, [F]).
facto_esta_numa_condicao(F, [avalia(F1)]) :-
    F =.. [H, H1 | _],
    F1 =.. [H, H1 | _].

% Verificar condições das regras
verifica_condicoes([nao avalia(X) e Y], [nao X | LF]) :-
    !,
    \+ avalia(_, X),
    verifica_condicoes([Y], LF).
verifica_condicoes([avalia(X) e Y], [N | LF]) :-
    !,
    avalia(N, X),
    verifica_condicoes([Y], LF).
verifica_condicoes([nao avalia(X)], [nao X]) :-
    !,
    \+ avalia(_, X).
verifica_condicoes([avalia(X)], [N]) :-
    !,
    avalia(N, X).
verifica_condicoes([nao X e Y], [nao X | LF]) :-
    !,
    \+ facto(_, X),
    verifica_condicoes([Y], LF).
verifica_condicoes([X e Y], [N | LF]) :-
    !,
    facto(N, X),
    verifica_condicoes([Y], LF).
verifica_condicoes([nao X], [nao X]) :-
    !,
    \+ facto(_, X).
verifica_condicoes([X], [N]) :-
    facto(N, X).

% Concluir ações das regras
concluir([cria_facto(F) | Y], ID, LFactos) :-
    !,
    cria_facto(F, ID, LFactos),
    concluir(Y, ID, LFactos).
concluir([remove_facto(F) | Y], ID, LFactos) :-
    !,
    retirar_facto(F),
    concluir(Y, ID, LFactos).
concluir([], _, _) :-
    !.

% Criar um novo facto
cria_facto(F, _, _) :-
    facto(_, F),
    !.
cria_facto(F, ID, LFactos) :-
    retract(ultimo_facto(N1)),
    N is N1 + 1,
    asserta(ultimo_facto(N)),
    assertz(justifica(N, ID, LFactos)),
    assertz(facto(N, F)),
    write('Foi concluído o facto nº '), write(N), write(' -> '), write(F), nl, !.

% Avaliar condições
avalia(N, P) :-
    P =.. [Functor, Entidade, Operando, Valor],
    P1 =.. [Functor, Entidade, Valor1],
    facto(N, P1),
    compara(Valor1, Operando, Valor).

% Comparar valores
compara(V1, ==, V) :- V1 == V.
compara(V1, \==, V) :- V1 \== V.
compara(V1, >, V) :- V1 > V.
compara(V1, <, V) :- V1 < V.
compara(V1, >=, V) :- V1 >= V.
compara(V1, =<, V) :- V1 =< V.

% Mostrar factos
mostra_factos :-
    findall(N, facto(N, _), LFactos),
    escreve_factos(LFactos).

% Gerar explicações do tipo "Como"
como(N) :-
    ultimo_facto(Last),
    Last < N,
    !,
    write('Essa conclusão não foi tirada'), nl, nl.
como(N) :-
    justifica(N, ID, LFactos),
    !,
    facto(N, F),
    write('Concluí o facto nº '), write(N), write(' -> '), write(F), nl,
    write('pela regra '), write(ID), nl,
    write('por se ter verificado que:'), nl,
    escreve_factos(LFactos),
    write('********************************************************'), nl,
    explica(LFactos).
como(N) :-
    facto(N, F),
    write('O facto nº '), write(N), write(' -> '), write(F), nl,
    write('foi conhecido inicialmente'), nl,
    write('********************************************************'), nl.

% Escrever factos
escreve_factos([I | R]) :-
    facto(I, F),
    !,
    write('O facto nº '), write(I), write(' -> '), write(F), write(' é verdadeiro'), nl,
    escreve_factos(R).
escreve_factos([I | R]) :-
    write('A condição '), write(I), write(' é verdadeira'), nl,
    escreve_factos(R).
escreve_factos([]).

% Explicar factos
explica([I | R]) :-
    \+ integer(I),
    !,
    explica(R).
explica([I | R]) :-
    como(I),
    explica(R).
explica([]) :-
    write('********************************************************'), nl.

% Gerar explicações do tipo "Porque não" (whynot)
whynot(Facto) :-
    whynot(Facto, 1).

whynot(Facto, _) :-
    facto(_, Facto),
    !,
    write('O facto '), write(Facto), write(' não é falso!'), nl.
whynot(Facto, Nivel) :-
    encontra_regras_whynot(Facto, LLPF),
    whynot1(LLPF, Nivel).
whynot(nao Facto, Nivel) :-
    formata(Nivel),
    write('Porque:'), write(' O facto '), write(Facto),
    write(' é verdadeiro'), nl.
whynot(Facto, Nivel) :-
    formata(Nivel),
    write('Porque:'), write(' O facto '), write(Facto),
    write(' não está definido na base de conhecimento'), nl.

% Encontrar regras para "whynot"
encontra_regras_whynot(Facto, LLPF) :-
    findall(
        (ID, LPF),
        (
            regra ID se LHS entao RHS,
            member(cria_facto(Facto), RHS),
            encontra_premissas_falsas(LHS, LPF),
            LPF \== []
        ),
        LLPF
    ).

% Explicar "whynot"
whynot1([], _).
whynot1([(ID, LPF) | LLPF], Nivel) :-
    formata(Nivel),
    write('Porque pela regra '), write(ID), write(':'), nl,
    Nivel1 is Nivel + 1,
    explica_porque_nao(LPF, Nivel1),
    whynot1(LLPF, Nivel).

% Encontrar premissas falsas
encontra_premissas_falsas([nao X e Y], LPF) :-
    verifica_condicoes([nao X], _),
    !,
    encontra_premissas_falsas([Y], LPF).
encontra_premissas_falsas([X e Y], LPF) :-
    verifica_condicoes([X], _),
    !,
    encontra_premissas_falsas([Y], LPF).
encontra_premissas_falsas([nao X], []) :-
    verifica_condicoes([nao X], _),
    !.
encontra_premissas_falsas([X], []) :-
    verifica_condicoes([X], _),
    !.
encontra_premissas_falsas([nao X e Y], [nao X | LPF]) :-
    !,
    encontra_premissas_falsas([Y], LPF).
encontra_premissas_falsas([X e Y], [X | LPF]) :-
    !,
    encontra_premissas_falsas([Y], LPF).
encontra_premissas_falsas([nao X], [nao X]) :-
    !.
encontra_premissas_falsas([X], [X]).
encontra_premissas_falsas([]).

% Explicar porque não
explica_porque_nao([], _).
explica_porque_nao([nao avalia(X) | LPF], Nivel) :-
    !,
    formata(Nivel),
    write('A condição nao '), write(X), write(' é falsa'), nl,
    explica_porque_nao(LPF, Nivel).
explica_porque_nao([avalia(X) | LPF], Nivel) :-
    !,
    formata(Nivel),
    write('A condição '), write(X), write(' é falsa'), nl,
    explica_porque_nao(LPF, Nivel).
explica_porque_nao([P | LPF], Nivel) :-
    formata(Nivel),
    write('A premissa '), write(P), write(' é falsa'), nl,
    Nivel1 is Nivel + 1,
    whynot(P, Nivel1),
    explica_porque_nao(LPF, Nivel).

% Formatar saída
formata(Nivel) :-
    Espacos is (Nivel - 1) * 5,
    tab(Espacos).

% Retirar um facto
retirar_facto(F) :-
    retract(facto(_, F)),
    findall(K1, (justifica(K1, _, L), member(facto(_, F), L)), LK1),
    retirar_lista_factos(LK1).

retirar_lista_factos([]).
retirar_lista_factos([K1 | LK1]) :-
    retract(justifica(K1, _, _)),
    retirar_facto(K1),
    retirar_lista_factos(LK1).

% Predicado 'diagnostico' inicia o processo de diagnóstico
diagnostico :-
    retractall(factos_processados(_)),
    diagnostico_loop.

% Loop que processa testes pendentes ou exibe o diagnóstico final
diagnostico_loop :-
    diagnostico_finalizado,
    !,
    mostrar_diagnostico.
diagnostico_loop :-
    problemas_pendentes,
    !,
    diag_problemas,
    ( arranca_motor -> true ; true ),
    diagnostico_loop.
diagnostico_loop :-
    write('Não foi possível chegar a um diagnóstico final.'), nl.

% Verificar se um diagnóstico final foi alcançado
diagnostico_finalizado :-
    facto(_, diagnostico(_, _)).

% Verificar se há problemas pendentes
problemas_pendentes :-
    facto(_, proximo_teste(_, _)).

% Processar todos os factos 'proximo_teste' pendentes
diag_problemas :-
    findall((Id, Veiculo, Teste), facto(Id, proximo_teste(Veiculo, Teste)), Tests),
    process_tests(Tests).

% Processar cada teste
process_tests([]).
process_tests([(Id, Veiculo, Teste) | Rest]) :-
    tratar_problema(Id, Veiculo, Teste),
    process_tests(Rest).

% Tratar cada problema individualmente
tratar_problema(Id, Veiculo, Teste) :-
    repeat,
    format('O ~w está com o problema: ~w? (sim/nao) ', [Veiculo, Teste]),
    read(Resposta),
    ( (Resposta == sim ; Resposta == nao) ->
        NovoFacto =.. [Teste, Veiculo, Resposta],
        retract(ultimo_facto(N1)),
        N is N1 + 1,
        asserta(ultimo_facto(N)),
        assertz(facto(N, NovoFacto)),
        retract(facto(Id, proximo_teste(Veiculo, Teste))),
        !
    ;   
        write('Resposta inválida. Por favor, responda com sim ou nao.'), nl,
        fail
    ).

% Exibir o diagnóstico final
mostrar_diagnostico :-
    findall((Veiculo, Diagnostico), facto(_, diagnostico(Veiculo, Diagnostico)), Diagnosticos),
    mostrar_diagnosticos(Diagnosticos).

mostrar_diagnosticos([]).
mostrar_diagnosticos([(Veiculo, Diagnostico) | Rest]) :-
    format('Diagnóstico para ~w: ~w~n', [Veiculo, Diagnostico]),
    mostrar_diagnosticos(Rest).
