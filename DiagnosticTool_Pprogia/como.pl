%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- encoding(utf8).
% Geracao de explicacoes do tipo "Como" para o diagnostico
como():-
    como(1).

como(N) :-
    NSeguinte is N+1,
    (   justifica(N, ID, _)
        ->  facto(N, F),
            % unifica o F
            F =.. [_, _, Regra],
            pergunta(F, PerguntaFormatada),
            formata(N),write('Pergunta: '), write(PerguntaFormatada), nl,
            formata(N),write('Resposta: '), write(Regra), nl,
            formata(N),write('Gerou o facto número '), write(N), write(' -> '), write(F), nl,
            formata(N),write('Disparando assim a regra '), write(ID), nl,
            % proximo facto
            facto(NSeguinte, FSeguinte),
            FSeguinte =.. [PerguntaSeguinte, _, RespostaSeguinte],
            % caso o proximo facto seja diagnostico o "como" acaba
            continua_como(NSeguinte, PerguntaSeguinte, RespostaSeguinte)
        ;   !
    ).

% caso o proximo facto seja diagnostico o "como" acaba
continua_como(NSeguinte, PerguntaSeguinte, RespostaSeguinte):-
    PerguntaSeguinte == diagnostico
        -> write('Diagnóstico: '), write(RespostaSeguinte)
        ;  como(NSeguinte).
		
como_response(N, Response) :-
    NSeguinte is N + 1,
    (   justifica(N, ID, _)
    ->  facto(N, F),
        F =.. [_, _, Regra],
        pergunta(F, PerguntaFormatada),
        % Transformar o termo composto em um átomo para JSON
        term_to_atom(F, FAtom),
        term_to_atom(Regra, RegraAtom),
        % Criação de uma estrutura de resposta em JSON para a pergunta atual
        Current = _{
            pergunta: PerguntaFormatada,
            resposta: RegraAtom,
            fato: FAtom,
            regra: ID
        },
        facto(NSeguinte, FSeguinte),
        FSeguinte =.. [PerguntaSeguinte, _, RespostaSeguinte],
        (   PerguntaSeguinte == diagnostico
        ->  term_to_atom(RespostaSeguinte, DiagnosticoAtom),
            Response = [Current, _{diagnostico: DiagnosticoAtom}]
        ;   como_response(NSeguinte, NextResponse),
            Response = [Current | NextResponse]
        )
    ;   Response = []
    ).
