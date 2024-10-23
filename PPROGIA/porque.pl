% Geracao de explicacoes do tipo "porque"
porque(Pergunta) :-
    regra_anterior(Pergunta),
    resposta_anterior(Pergunta).

porque(_) :-
    write('A pergunta não foi feita anteriormente ou não existe resposta.'), nl.

% Predicado auxiliar para encontrar a regra corretamente
regra_anterior(Pergunta) :-
    regra ID se LHS entao RHS,
    LHS = [Pergunta | _],
    write(ID).

% Predicado auxiliar para encontrar a resposta correspondente
resposta_anterior(Pergunta) :-
    facto(_, F),
    F =.. [Pergunta, _, Resposta],
    write(Resposta).
