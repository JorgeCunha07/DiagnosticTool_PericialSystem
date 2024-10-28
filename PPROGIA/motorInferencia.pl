% Motor de inferencia

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
:- dynamic carro_selecionado/1.
:- dynamic carro_numero_selecionado/1.

:- consult('bc.pl').
:- consult('escolha_carro.pl').
:- consult('diagnostico.pl').
:- consult('como.pl').
:- consult('porque.pl').
:- consult('porque_nao.pl').

% Carregar a base de conhecimento
carrega_bc :-
    write('NOME DA BASE DE CONHECIMENTO (terminar com .)-> '),
    read(NBC),
    consult(NBC).

% Iniciar o motor de inferencia
% arranca_motor:- facto(N,Facto),
%                facto_dispara_regras1(Facto, LRegras),
%                dispara_regras(N, Facto, LRegras),
%                ultimo_facto(N).

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
% Caso em que o facto nao origina o disparo de qualquer regra.
facto_dispara_regras1(_, []).

% Disparar regras baseadas no facto
dispara_regras(N, Facto, [ID | LRegras]) :-
    regra ID se LHS entao RHS,
    facto_esta_numa_condicao(Facto, LHS),
	% Instancia Facto em LHS
    verifica_condicoes(LHS, LFactos),
    member(N, LFactos),
    sort(LFactos, SortedLFactos),
    (   regra_disparadas(ID, SortedLFactos) ->
		% Regra ja foi disparada com estas condicoes; ignore
        true
    ;   
		% Dispara a regra e registra que foi disparada com estas condicoes
        concluir(RHS, ID, LFactos),
        asserta(regra_disparadas(ID, SortedLFactos))
    ),
    !,
    dispara_regras(N, Facto, LRegras).

dispara_regras(N, Facto, [_ | LRegras]) :-
    dispara_regras(N, Facto, LRegras).

dispara_regras(_, _, []).

% Verificar se o facto esta numa condicao
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

% Verificar condicoes das regras
verifica_condicoes([nao avalia(X) e Y], [nao X | LF]) :-
    !,
    \+ avalia(_, X),
    verifica_condicoes([Y], LF).

verifica_condicoes([X e nao avalia(Y)], [nao X | LF]) :-
    !,
    \+ avalia(_, Y),
    verifica_condicoes([X], LF).

verifica_condicoes([avalia(X) e Y], [N | LF]) :-
    !,
    avalia(N, X),
    verifica_condicoes([Y], LF).

verifica_condicoes([X e avalia(Y)], [N | LF]) :-
    !,
    avalia(N, Y),
    verifica_condicoes([X], LF).

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

% Concluir acoes das regras
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

cria_facto(F, ID, LFactos) :-
    (
    %caso crie um facto proximo_teste
    F = proximo_teste(_, _) ->
        ultimo_facto(N),
        assertz(facto(N, F)),
        assertz(justifica(N, ID, LFactos)),
        write('Foi concluido o facto proximo_teste numero '), write(N), write(' -> '), write(F), nl
    ;
    F = solucao(_, _) ->
        retract(ultimo_facto(N1)),
        N is N1 + 1,
        asserta(ultimo_facto(N)),
        assertz(facto(N, F)),
        write('Foi concluido o facto numero '), write(N), write(' -> '), write(F), nl
    ;
    % caso crie um facto diagnostico
        retract(ultimo_facto(N1)),
        N is N1 + 1,
        asserta(ultimo_facto(N)),
        assertz(justifica(N1, ID, LFactos)),
        assertz(facto(N, F)),
        write('Foi concluido o facto numero '), write(N), write(' -> '), write(F), nl
    ),
    !.

% Avaliar condicoes
avalia(N, P) :-
    P =.. [Functor, Entidade, Operando, MinOuMax],
    P1 =.. [Functor, Entidade, ValorReal],
    facto(N, P1),
    P2 =.. [Functor, Entidade, ValorMin, ValorMax, _],
    call(P2),
    ( MinOuMax == 'Min' -> Valor = ValorMin ; Valor = ValorMax ),
    compara(ValorReal, Operando, Valor).

% Comparar valores
compara(V1, ==, V) :- V1 == V.
compara(V1, \==, V) :- V1 \== V.
compara(V1, >, V) :- V1 > V.
compara(V1, <, V) :- V1 < V.
compara(V1, >=, V) :- V1 >= V.
compara(V1, =<, V) :- V1 =< V.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Visualizacao da base de factos
mostra_factos :-
    findall(N, facto(N, _), LFactos),
    escreve_factos(LFactos).

% Visualizacao das justificacoes
mostra_justificacoes :-
    findall([N,ID], justifica(N, ID, _), LJustifica),
    write(LJustifica).

% Escrever factos
escreve_factos([I | R]) :-
    facto(I, F),
    !,
    write('O facto numero '), write(I), write(' -> '), write(F), write(' e verdadeiro'), nl,
    escreve_factos(R).
escreve_factos([I | R]) :-
    write('A condicao '), write(I), write(' e verdadeira'), nl,
    escreve_factos(R).
escreve_factos([]).
