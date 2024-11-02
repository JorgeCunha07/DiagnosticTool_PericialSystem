% Predicado que inicia o processo de diagnostico
diagnostico :-
    retractall(factos_processados(_)),
    diagnostico_continuo.
	
diagnostico_passo_passo_con :-
    diag_problemas,
	arranca_motor.
	
% Predicado que inicia APENAS uma pergunta do diagnostico
diagnostico_passo_passo(Resposta) :-
    diag_problemas_passo_passo(Resposta),
	arranca_motor.

% Loop que processa testes pendentes ou exibe o diagnostico final
diagnostico_continuo :-
    diagnostico_finalizado,
    !,
    mostrar_diagnostico,
    mostrar_solucao.

diagnostico_continuo :-
    problemas_pendentes,
    !,
    diag_problemas,
    ( arranca_motor -> true ; true ),
    diagnostico_continuo.

diagnostico_continuo :-
    write('Não foi possivel chegar a um diagnóstico final.'), nl.

% Verificar se um diagnostico final foi alcancado
diagnostico_finalizado :-
    facto(_, diagnostico(_, _)).

% Verificar se ha problemas pendentes
problemas_pendentes :-
    facto(_, proximo_teste(_, _)).

% Processar todos os factos 'proximo_teste' pendentes
diag_problemas :-
    findall((Id, Veiculo, Teste), facto(Id, proximo_teste(Veiculo, Teste)), Tests),
    processar_testes(Tests).

% Processar todos os factos 'proximo_teste' pendentes
diag_problemas_passo_passo(Resposta) :-
    findall((Id, Veiculo, Teste), facto(Id, proximo_teste(Veiculo, Teste)), Tests),
    processar_testes_passo_passo(Tests, Resposta).

% Processar cada teste
processar_testes([]).
processar_testes([(Id, Veiculo, Teste) | Resto]) :-
    tratar_problema(Id, Veiculo, Teste),
    processar_testes(Resto).

% Processar cada teste
processar_testes_passo_passo([(Id, Veiculo, Teste) | _], Resposta) :-
    tratar_problema_passo_passo(Id, Veiculo, Teste, Resposta).

% Tratar cada problema individualmente
tratar_problema(Id, Veiculo, Teste) :-
    TesteTermo =.. [Teste, Veiculo, _],
    (perguntavel(TesteTermo) ->
        pergunta(TesteTermo, Pergunta),
        opcoes_validas(TesteTermo, OpcoesValidas),
        format('~w ~w ', [Pergunta, OpcoesValidas]),
        repeat,
        read(Resposta),
        (
            % Verificar se OpcoesValidas define um intervalo numérico
            OpcoesValidas = [Min, Max],
            number(Min), number(Max),
            number(Resposta) ->
                (Resposta >= Min, Resposta =< Max ->
                    NovoFacto =.. [Teste, Veiculo, Resposta],
                    retract(ultimo_facto(N1)),
                    N is N1 + 1,
                    asserta(ultimo_facto(N)),
                    assertz(facto(N, NovoFacto)),
                    retract(facto(Id, proximo_teste(Veiculo, Teste))),
                    !
                ;
                    write('Resposta inválida. Insira um número entre '), write(Min), write(' e '), write(Max), nl,
                    fail
                )
            ;
            % Caso contrário, verificar resposta nas opções válidas diretamente
            (member(Resposta, OpcoesValidas) ->
                NovoFacto =.. [Teste, Veiculo, Resposta],
                retract(ultimo_facto(N1)),
                N is N1 + 1,
                asserta(ultimo_facto(N)),
                assertz(facto(N, NovoFacto)),
                retract(facto(Id, proximo_teste(Veiculo, Teste))),
                !
            ;
                write('Resposta inválida. Por favor, responda com uma das opções: '), write(OpcoesValidas), nl,
                fail
            )
        )
    ;
        write('Teste não é perguntável: '), write(Teste), nl
    ).

% Tratar cada problema individualmente
tratar_problema_passo_passo(Id, Veiculo, Teste, Resposta) :-
    TesteTermo =.. [Teste, Veiculo, _],
    perguntavel(TesteTermo),
    opcoes_validas(TesteTermo, OpcoesValidas),
    validar_resposta(OpcoesValidas, Resposta),
    NovoFacto =.. [Teste, Veiculo, Resposta],
    retract(ultimo_facto(N1)),
    N is N1 + 1,
    asserta(ultimo_facto(N)),
    assertz(facto(N, NovoFacto)),
    retract(facto(Id, proximo_teste(Veiculo, Teste))).

validar_resposta([sim, nao], Resposta) :-
    member(Resposta, [sim, nao]).

validar_resposta([Min, Max], Resposta) :-
    number(Resposta),
    Resposta >= Min,
    Resposta =< Max.

% Exibir o diagnostico final
mostrar_diagnostico :-
    facto(_, diagnostico(_, Diagnostico)),
    carro_selecionado(Info),
    format('Diagnóstico para ~w: ~w~n', [Info, Diagnostico]).

% Exibir a solução final
mostrar_solucao :-
    facto(_, solucao(_, Solucao)),
    carro_selecionado(Info),
    format('Solução para ~w: ~w~n', [Info, Solucao]).
	
% Exibir o diagnostico e a solucao
mostra_diagnostico_solucao(Diagnostico, Solucao) :-
	findall(D, facto(_, diagnostico(_, D)), Diagnostico),
    findall(S, facto(_, solucao(_, S)), Solucao).

% Predicado que retorna todos os diagnósticos possíveis
listar_diagnosticos_possiveis(Diagnosticos) :-
    listar_diagnosticos_possiveis_aux(Diagnosticos, []).

% Predicado auxiliar com acumulador
listar_diagnosticos_possiveis_aux(Diagnosticos, Acumulador) :-
    regra _ se _ entao RHS,
    member(cria_facto(diagnostico(_, Diagnostico)), RHS),
    \+ member(Diagnostico, Acumulador), % Evitar duplicados
    !,
    listar_diagnosticos_possiveis_aux(Diagnosticos, [Diagnostico | Acumulador]).
listar_diagnosticos_possiveis_aux(Diagnosticos, Diagnosticos).

% Predicado para obter todas as perguntas
obter_perguntas(Testes) :-
 findall(P, facto(_, proximo_teste(_, P)), Testes).
 
% Predicado para criar uma pergunta com as respostas validas
criacao_pergunta(Teste, Pergunta, OpcoesValidas) :-
	functor(TesteTermo, Teste, 2),
    pergunta(TesteTermo, Pergunta),
    opcoes_validas(TesteTermo, OpcoesValidas).
