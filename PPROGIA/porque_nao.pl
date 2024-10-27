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
        Nivel =:= 1 ->
        formata(Nivel),
        write('Porque:'), write(' O facto '), write(Facto),
        write(' nao esta definido na base de conhecimento'), nl
    ;   
        true
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

% Explicar porque nao
explica_porque_nao([], _).
explica_porque_nao([nao avalia(X) | LPF], Nivel) :-
    !,
    formata(Nivel),
    write('A condicao nao '), write(X), write(' e falsa'), nl,
    explica_porque_nao(LPF, Nivel).
explica_porque_nao([avalia(X) | LPF], Nivel) :-
    !,
    formata(Nivel),
    write('A condicao '), write(X), write(' e falsa'), nl,
    explica_porque_nao(LPF, Nivel).
explica_porque_nao([P | LPF], Nivel) :-
    formata(Nivel),
    write('A premissa '), write(P), write(' e falsa'), nl,
    Nivel1 is Nivel + 1,
    whynot(P, Nivel1),
    explica_porque_nao(LPF, Nivel).

% Formatar saida
formata(Nivel) :-
    Espacos is (Nivel - 1) * 5,
    tab(Espacos).

% Define todos os diagnósticos possíveis a partir das regras
todos_diagnosticos_possiveis([
    'Nivel de oleo insuficiente.',
    'Excesso de oleo.',
    'Falha nas velas de ignição',
    'Fuga no sistema de arrefecimento.',
    'Filtro de combustível entupido.',
    'Injeção de combustivel com defeito.',
    'Amortecedores gastos.',
    'Sistema de ABS com defeito.',
    'Pastilhas de travao gastas.',
    'Discos de travão gastos.',
    'Falta de combustivel ou bomba de combustivel com defeito',
    'Verificacao adicional necessaria.',
    'Problema indeterminado.',
    'Sensor dor motor com falha.',
    'Alternador com defeito.',
    'Compressor de AC com defeito.',
    'Falha no motor de arranque.',
    'Sistema de ignicao com defeito.',
    'Problema no radiador ou ventoinha.',
    'Sem barulhos anormais.',
    'Servofreio defeituoso.',
    'Falha na bomba de agua.',
    'Vazamento ou bloqueio no sistema de escape.',
    'Embraiagem com desgaste excessivo ou volante bimassa defeituoso.',
    'Problema nos pneus, jantes ou calcos de travao.',
    'Correia dentada gasta.',
    'Bateria com defeito ou sem carga.',
    'Problema nao identificado.',
    'Nenhum problema identificado.'
]).

% Predicado que retorna todos os diagnósticos possíveis
listar_diagnosticos_possiveis(Diagnosticos) :-
    todos_diagnosticos_possiveis(Diagnosticos).