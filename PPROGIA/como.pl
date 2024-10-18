%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Geracao de explicacoes do tipo "Como"
como(N) :-
    ultimo_facto(Last),
    Last < N,
    !,
    write('Essa conclusao nao foi tirada'), nl, nl.
como(N) :-
    justifica(N, ID, LFactos),
    !,
    facto(N, F),
    write('Conclui o facto numero '), write(N), write(' -> '), write(F), nl,
    write('pela regra '), write(ID), nl,
    write('por se ter verificado que:'), nl,
    escreve_factos(LFactos),
    write('********************************************************'), nl,
    explica(LFactos).
como(N) :-
    facto(N, F),
    write('O facto numero '), write(N), write(' -> '), write(F), nl,
    write('foi conhecido inicialmente'), nl,
    write('********************************************************'), nl.

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
