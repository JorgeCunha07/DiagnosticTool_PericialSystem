%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Geracao de explicacoes do tipo "Porque"
% Exemplo: ?- porque(classe(meu_veiculo,ligeiro)).
porque(Facto) :-
    (   facto(N, Facto)
    ->  (N == 1
        ->  write('O facto '), write(Facto), write(' é verdadeiro visto ser a primeira pergunta')
        ;   Facto =.. [_, _, Resposta],
            NAnterior is N - 1,
            justifica(NAnterior, ID, _),
            facto(NAnterior, FactoAnterior),
            FactoAnterior =.. [_, _, RespostaAnterior],
            pergunta(FactoAnterior, PerguntaAnteriorFormatada),
            write('O facto '), write(Facto),
            write(' é verdadeiro porque pela regra '), write(ID),
            write(' foi possível concluir que o utilizador respondeu: '), write(RespostaAnterior),
            write(' à pergunta: '), write(PerguntaAnteriorFormatada), nl,
            write('Com isto foi necessário realizar a seguinte pergunta: '),
            pergunta(Facto, PerguntaFormatada),
            write(PerguntaFormatada), write(' ao qual o mesmo respondeu: '), write(Resposta),
            write(' mostrando que o facto '), write(Facto), write(' é verdadeiro')
        )
    ;   write('O facto '), write(Facto), write(' não é verdadeiro')
    ).
