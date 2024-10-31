%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Geracao de explicacoes do tipo "Porque nao"
% Exemplo: ?- whynot(classe(meu_veiculo,ligeiro)).
whynot(Facto) :-
    whynot(Facto, 1).

whynot(Facto, _) :-
    facto(_, Facto),
    !,
    write('O facto '), write(Facto), write(' nao e falso!'), nl.
whynot(Facto, Nivel) :-
    encontra_regras_whynot(Facto, LLPF),
    (
        LLPF \= [] ->
        whynot1(LLPF, Nivel)
    ;
        formata(Nivel),
        %write('Porque: O facto '), write(Facto),
        %write(' não esta definido na base de conhecimento'), nl
		write('Parou no nivel: '), write(Nivel)
    ).


whynot(nao Facto, Nivel) :-
    formata(Nivel),
    write('Porque:'), write(' O facto '), write(Facto),
    write(' e verdadeiro'), nl.
whynot(Facto, Nivel) :-
    formata(Nivel),
    write('Porque:'), write(' O facto '), write(Facto),
    write(' nao esta definido na base de conhecimento'), nl.

%  As explicacoes do whynot(Facto) devem considerar todas as regras que poderiam dar origem a conclusao relativa ao facto Facto
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
whynot1([(ID, LPF) | _], Nivel) :-
    formata(Nivel),
    write('Porque pela regra '), write(ID), write(':'), nl,
    Nivel1 is Nivel + 1,
    explica_porque_nao(LPF, Nivel1),
    !.

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

% Explicar porque nao
explica_porque_nao([], _).
explica_porque_nao([P | _], Nivel) :-
    formata(Nivel),
    write('A premissa '), write(P), write(' e falsa'), nl,
    functor(P, TesteAnterior, _),
    Nivel1 is Nivel + 1,
    
    % Verifica se o TesteAnterior é 'avalia' e manipula de acordo
    ( TesteAnterior == avalia ->
        P =.. [_, InnerTerm | _],      % Decompor para obter o termo dentro de avalia(...)
        functor(InnerTerm, Functor, _), % Extrair o nome do functor (e.g., fluido_transmissao)
        ProximoFacto = proximo_teste(_, Functor)  % Define ProximoFacto com o functor extraído
    ;
        % Caso contrário, usa TesteAnterior diretamente
        ProximoFacto = proximo_teste(_, TesteAnterior)
    ),
    
    % Continua com o próximo diagnóstico
    whynot(ProximoFacto, Nivel1).


% Formatar saida
formata(Nivel) :-
    Espacos is (Nivel - 1) * 5,
    tab(Espacos).
