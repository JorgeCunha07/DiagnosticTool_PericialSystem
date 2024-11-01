:-encoding(utf8).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Geracao de explicacoes do tipo "Porque"
% Exemplo: ?- porque(classe(meu_veiculo,ligeiro)).
porque(Facto):-
    facto(1, Facto),
    write('O facto '), write(Facto), write(' é verdadeiro visto ser a resposta a primeira pergunta').

porque(Facto):-
    facto(N, Facto),
    Facto =.. [_, _, Resposta],
    NAnterior is N-1,
    justifica(NAnterior, ID, _),
    facto(NAnterior, FactoAnterior),
    FactoAnterior =.. [_, _, RespostaAnterior],
    pergunta(FactoAnterior, PerguntaAnteriorFormatada),
    write('O facto '), write(Facto),
    write(' é verdadeiro porque pela regra '), write(ID),
    write(' foi possivel concluir que o utilizador respondeu: "'), write(RespostaAnterior),
    write('" à pergunta: '), write(PerguntaAnteriorFormatada), nl,
    write('Com isto foi necessário realizar a seguinte pergunta: '),
    pergunta(Facto, PerguntaFormatada),
    write(PerguntaFormatada), write(' ao qual o mesmo respondeu: "'), write(Resposta),
    write('" mostrando que o facto '), write(Facto), write(' é verdadeiro').

porque(Facto):-
    write('O facto '), write(Facto), write(' não existe.').

porque_response(Facto, Explicacao) :-
    (   call(facto(N, Facto))
    ->  (N == 1
        ->  term_to_atom(Facto, FactoAtom),
            Explicacao = _{fato: FactoAtom, explicacao: "O facto é verdadeiro visto ser a primeira pergunta"}
        ;   Facto =.. [_, _, Resposta],
            NAnterior is N - 1,
            justifica(NAnterior, ID, _),
            call(facto(NAnterior, FactoAnterior)),
            FactoAnterior =.. [_, _, RespostaAnterior],
            pergunta(FactoAnterior, PerguntaAnteriorFormatada),
            pergunta(Facto, PerguntaFormatada),
            % Converter termos compostos em átomos para JSON
            term_to_atom(Facto, FactoAtom),
            term_to_atom(Resposta, RespostaAtom),
            term_to_atom(RespostaAnterior, RespostaAnteriorAtom),
            Explicacao = _{
                fato: FactoAtom,
                explicacao: "O facto e verdadeiro",
                regra: ID,
                resposta_anterior: RespostaAnteriorAtom,
                pergunta_anterior: PerguntaAnteriorFormatada,
                pergunta: PerguntaFormatada,
                resposta: RespostaAtom
            }
        )
    ;   term_to_atom(Facto, FactoAtom),
        Explicacao = _{fato: FactoAtom, explicacao: "O facto não é verdadeiro"}
    ).
