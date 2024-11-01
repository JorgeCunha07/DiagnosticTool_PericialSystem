%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- encoding(utf8).
% Geração de explicacões do tipo "Porque não"
% Exemplo: ?- porque_nao(classe(meu_veiculo,ligeiro)).
porque_nao(Facto) :-
    porque_nao(Facto, 1).

porque_nao(Facto, _) :-
    facto(_, Facto),
    !,
    write('O facto '), write(Facto), write(' não é falso!'), nl.
porque_nao(Facto, Nivel) :-
    encontra_regras_porque_nao(Facto, LLPF),
    (
        LLPF \= [] ->
        porque_nao1(LLPF, Nivel)
    ;
        formata(Nivel),
        %write('Porque: O facto '), write(Facto),
        %write(' não está definido na base de conhecimento'), nl
		write('Parou no nível: '), write(Nivel)
    ).


porque_nao(nao Facto, Nivel) :-
    formata(Nivel),
    write('Porque:'), write(' O facto '), write(Facto),
    write(' é verdadeiro'), nl.
porque_nao(Facto, Nivel) :-
    formata(Nivel),
    write('Porque:'), write(' O facto '), write(Facto),
    write(' não esta definido na base de conhecimento'), nl.

%  As explicacoes do porque_nao(Facto) devem considerar todas as regras que poderiam dar origem a conclusao relativa ao facto Facto
encontra_regras_porque_nao(Facto, LLPF) :-
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

% Explicar "porque_nao"
porque_nao1([], _).
porque_nao1([(ID, LPF) | _], Nivel) :-
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
    write('A premissa '), write(P), write(' é falsa'), nl,
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
    porque_nao(ProximoFacto, Nivel1).


% Formatar saida
formata(Nivel) :-
    Espacos is (Nivel - 1) * 5,
    tab(Espacos).
