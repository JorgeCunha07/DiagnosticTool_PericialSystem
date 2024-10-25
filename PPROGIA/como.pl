%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
            write('pergunta ao utilizador: '), write(PerguntaFormatada), nl,
            write('ao qual o mesmo respondeu: '), write(Regra), nl,
            write('concluindo-se o facto numero '), write(N), write(' -> '), write(F), nl,
            write('disparando assim a regra '), write(ID),
            % proximo facto
            facto(NSeguinte, FSeguinte),
            FSeguinte =.. [PerguntaSeguinte, _, RespostaSeguinte],
            % caso o proximo facto seja diagnostico o "como" acaba
            ( PerguntaSeguinte == diagnostico
                -> write(' que gerou o seguinte diagnostico: '), write(RespostaSeguinte)
                ;  write(' criando a seguinte '), como(NSeguinte)
            )
        ;   !
    ).
