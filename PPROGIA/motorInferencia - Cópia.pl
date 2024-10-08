% Versão preparada para lidar com regras que contenham negação (nao)
% Metaconhecimento
% Usar base de conhecimento veIculos2.txt
% Explicações como?(how?) e porque não?(whynot?)

:-op(220,xfx,entao).
:-op(35,xfy,se).
:-op(240,fx,regra).
:-op(500,fy,nao).
:-op(600,xfy,e).

:-dynamic facto/2, justifica/3, regra_fired/2, factos_processados/1.

carrega_bc:- 
	write('NOME DA BASE DE CONHECIMENTO (terminar com .)-> '),
	read(NBC),
	consult(NBC).

% Modificacao do predicado arranca_motor para processar apenas factos novos
arranca_motor :-
    retractall(factos_processados(_)),
    processa_novo_factos.

processa_novo_factos :-
    findall((N, Facto), (facto(N, Facto), \+ factos_processados(N)), NovoFactos),
    ( NovoFactos \= [] ->
        processa_factos(NovoFactos),
        processa_novo_factos
    ;   true).

processa_factos([]).
processa_factos([(N, Facto)|Resto]) :-
    asserta(factos_processados(N)),
    facto_dispara_regras1(Facto, LRegras),
    dispara_regras(N, Facto, LRegras),
    processa_factos(Resto).



%%%%%%%%%%%
% Predicate 'diagnostico' starts the diagnostic process
diagnostico :-
    diagnostico_loop.

% Loop that processes pending tests or displays the final diagnosis
diagnostico_loop :-
    diagnostico_finalizado, !,
    mostrar_diagnostico.
diagnostico_loop :-
    problemas_pendentes, !,
    diag_problemas,
    ( arranca_motor -> true ; true ),
    diagnostico_loop.
diagnostico_loop :-
    write('Não foi possível chegar a um diagnóstico final.'), nl.

% Checks if a final diagnosis has been reached
diagnostico_finalizado :-
    facto(_, diagnostico(_, _)).

% Checks if there are pending problems to test
problemas_pendentes :-
    facto(_, proximo_teste(_, _)).

% Processes all pending 'proximo_teste' facts
diag_problemas :-
    findall((Id, Veiculo, Teste), facto(Id, proximo_teste(Veiculo, Teste)), Tests),
    process_tests(Tests).

% Processes each test
process_tests([]).
process_tests([(Id, Veiculo, Teste) | Resto]) :-
    tratar_problema(Id, Veiculo, Teste),
    process_tests(Resto).

% Handles individual problem
tratar_problema(Id, Veiculo, Teste) :-
    format('O ~w está com o problema: ~w? (sim/nao) ', [Veiculo, Teste]),
    read(Resposta),
    NovoFacto =.. [Teste, Veiculo, Resposta],
    retract(ultimo_facto(N1)),
    N is N1 + 1,
    asserta(ultimo_facto(N)),
    assertz(facto(N, NovoFacto)),
    retract(facto(Id, proximo_teste(Veiculo, Teste))).

% Displays the final diagnosis
mostrar_diagnostico :-
    findall((Veiculo, Diagnostico), facto(_, diagnostico(Veiculo, Diagnostico)), Diagnosticos),
    mostrar_diagnosticos(Diagnosticos).

mostrar_diagnosticos([]).
mostrar_diagnosticos([(Veiculo, Diagnostico)|Resto]) :-
    format('Diagnóstico para ~w: ~w~n', [Veiculo, Diagnostico]),
    mostrar_diagnosticos(Resto).


%%%%%%%%%%%


facto_dispara_regras1(Facto, LRegras):-
	facto_dispara_regras(Facto, LRegras),
	!.
facto_dispara_regras1(_, []).
% Caso em que o facto não origina o disparo de qualquer regra.

dispara_regras(N, Facto, [ID|LRegras]):-
    regra ID se LHS entao RHS,
    facto_esta_numa_condicao(Facto,LHS),
    verifica_condicoes(LHS, LFactos),
    member(N,LFactos),
    (   regra_fired(ID, LFactos) ->
        % Rule has already been fired with these facts; skip
        true
    ;   % Fire the rule and record that it has been fired with these facts
        concluir(RHS,ID,LFactos),
        assertz(regra_fired(ID, LFactos))
    ),
    !,
    dispara_regras(N, Facto, LRegras).

dispara_regras(N, Facto, [ID|LRegras]):-
    regra ID se LHS entao RHS e RHS2,
    facto_esta_numa_condicao(Facto,LHS),
    verifica_condicoes(LHS, LFactos),
    member(N,LFactos),
    (   regra_fired(ID, LFactos) ->
        % Rule has already been fired with these facts; skip
        true
    ;   % Fire the rule and record that it has been fired with these facts
        concluir(RHS,ID,LFactos),
        concluir(RHS2,ID,LFactos),
        assertz(regra_fired(ID, LFactos))
    ),
    !,
    dispara_regras(N, Facto, LRegras).

dispara_regras(N, Facto, [_|LRegras]):-
    dispara_regras(N, Facto, LRegras).

dispara_regras(_, _, []).



facto_esta_numa_condicao(F,[F  e _]).

facto_esta_numa_condicao(F,[avalia(F1)  e _]):- F=..[H,H1|_],F1=..[H,H1|_].

facto_esta_numa_condicao(F,[_ e Fs]):- facto_esta_numa_condicao(F,[Fs]).

facto_esta_numa_condicao(F,[F]).

facto_esta_numa_condicao(F,[avalia(F1)]):-F=..[H,H1|_],F1=..[H,H1|_].


verifica_condicoes([nao avalia(X) e Y],[nao X|LF]):- !,
	\+ avalia(_,X),
	verifica_condicoes([Y],LF).
verifica_condicoes([avalia(X) e Y],[N|LF]):- !,
	avalia(N,X),
	verifica_condicoes([Y],LF).

verifica_condicoes([nao avalia(X)],[nao X]):- !, \+ avalia(_,X).
verifica_condicoes([avalia(X)],[N]):- !, avalia(N,X).

verifica_condicoes([nao X e Y],[nao X|LF]):- !,
	\+ facto(_,X),
	verifica_condicoes([Y],LF).
verifica_condicoes([X e Y],[N|LF]):- !,
	facto(N,X),
	verifica_condicoes([Y],LF).

verifica_condicoes([nao X],[nao X]):- !, \+ facto(_,X).
verifica_condicoes([X],[N]):- facto(N,X).



concluir([cria_facto(F)|Y],ID,LFactos):-
	!,
	cria_facto(F,ID,LFactos),
	concluir(Y,ID,LFactos).

concluir([],_,_):-!.



cria_facto(F,_,_):-
	facto(_,F),!.

cria_facto(F,ID,LFactos):-
	retract(ultimo_facto(N1)),
	N is N1+1,
	asserta(ultimo_facto(N)),
	assertz(justifica(N,ID,LFactos)),
	assertz(facto(N,F)),
	write('Foi concluído o facto nº '),write(N),write(' -> '),write(F),get0(_),!.



avalia(N,P):-	P=..[Functor,Entidade,Operando,Valor],
		P1=..[Functor,Entidade,Valor1],
		facto(N,P1),
		compara(Valor1,Operando,Valor).

compara(V1,==,V):- V1==V.
compara(V1,\==,V):- V1\==V.
compara(V1,>,V):-V1>V.
compara(V1,<,V):-V1<V.
compara(V1,>=,V):-V1>=V.
compara(V1,=<,V):-V1=<V.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Visualização da base de factos

mostra_factos:-
	findall(N, facto(N, _), LFactos),
	escreve_factos(LFactos).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Geração de explicações do tipo "Como"

como(N):-ultimo_facto(Last),Last<N,!,
	write('Essa conclusão não foi tirada'),nl,nl.
como(N):-justifica(N,ID,LFactos),!,
	facto(N,F),
	write('Conclui o facto nº '),write(N),write(' -> '),write(F),nl,
	write('pela regra '),write(ID),nl,
	write('por se ter verificado que:'),nl,
	escreve_factos(LFactos),
	write('********************************************************'),nl,
	explica(LFactos).
como(N):-facto(N,F),
	write('O facto nº '),write(N),write(' -> '),write(F),nl,
	write('foi conhecido inicialmente'),nl,
	write('********************************************************'),nl.


escreve_factos([I|R]):-facto(I,F), !,
	write('O facto nº '),write(I),write(' -> '),write(F),write(' é verdadeiro'),nl,
	escreve_factos(R).
escreve_factos([I|R]):-
	write('A condição '),write(I),write(' é verdadeira'),nl,
	escreve_factos(R).
escreve_factos([]).

explica([I|R]):- \+ integer(I),!,explica(R).
explica([I|R]):-como(I),
		explica(R).
explica([]):-	write('********************************************************'),nl.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Geração de explicações do tipo "Porque nao"
% Exemplo: ?- whynot(classe(meu_veículo,ligeiro)).

whynot(Facto):-
	whynot(Facto,1).

whynot(Facto,_):-
	facto(_, Facto),
	!,
	write('O facto '),write(Facto),write(' não é falso!'),nl.
whynot(Facto,Nivel):-
	encontra_regras_whynot(Facto,LLPF),
	whynot1(LLPF,Nivel).
whynot(nao Facto,Nivel):-
	formata(Nivel),write('Porque:'),write(' O facto '),write(Facto),
	write(' é verdadeiro'),nl.
whynot(Facto,Nivel):-
	formata(Nivel),write('Porque:'),write(' O facto '),write(Facto),
	write(' não está definido na base de conhecimento'),nl.

%  As explicações do whynot(Facto) devem considerar todas as regras que poderiam dar origem a conclusão relativa ao facto Facto

encontra_regras_whynot(Facto,LLPF):-
	findall((ID,LPF),
		(
		regra ID se LHS entao RHS,
		member(cria_facto(Facto),RHS),
		encontra_premissas_falsas(LHS,LPF),
		LPF \== []
		),
		LLPF).

whynot1([],_).
whynot1([(ID,LPF)|LLPF],Nivel):-
	formata(Nivel),write('Porque pela regra '),write(ID),write(':'),nl,
	Nivel1 is Nivel+1,
	explica_porque_nao(LPF,Nivel1),
	whynot1(LLPF,Nivel).

encontra_premissas_falsas([nao X e Y], LPF):-
	verifica_condicoes([nao X], _),
	!,
	encontra_premissas_falsas([Y], LPF).
encontra_premissas_falsas([X e Y], LPF):-
	verifica_condicoes([X], _),
	!,
	encontra_premissas_falsas([Y], LPF).
encontra_premissas_falsas([nao X], []):-
	verifica_condicoes([nao X], _),
	!.
encontra_premissas_falsas([X], []):-
	verifica_condicoes([X], _),
	!.
encontra_premissas_falsas([nao X e Y], [nao X|LPF]):-
	!,
	encontra_premissas_falsas([Y], LPF).
encontra_premissas_falsas([X e Y], [X|LPF]):-
	!,
	encontra_premissas_falsas([Y], LPF).
encontra_premissas_falsas([nao X], [nao X]):-!.
encontra_premissas_falsas([X], [X]).
encontra_premissas_falsas([]).

explica_porque_nao([],_).
explica_porque_nao([nao avalia(X)|LPF],Nivel):-
	!,
	formata(Nivel),write('A condição nao '),write(X),write(' é falsa'),nl,
	explica_porque_nao(LPF,Nivel).
explica_porque_nao([avalia(X)|LPF],Nivel):-
	!,
	formata(Nivel),write('A condição '),write(X),write(' é falsa'),nl,
	explica_porque_nao(LPF,Nivel).
explica_porque_nao([P|LPF],Nivel):-
	formata(Nivel),write('A premissa '),write(P),write(' é falsa'),nl,
	Nivel1 is Nivel+1,
	whynot(P,Nivel1),
	explica_porque_nao(LPF,Nivel).

formata(Nivel):-
	Esp is (Nivel-1)*5, tab(Esp).

retirar_facto(K):-
	retract(facto(K,_)),
	findall(K1,(justifica(K1,_,L),member(K,L)),LK1),
	retirar_lista_factos(LK1),
	atualiza_ultimo_facto.

retirar_lista_factos([ ]).
retirar_lista_factos([K1|LK1]):-
	retract(justifica(K1,_,_)),
	retirar_facto(K1),
	retirar_lista_factos(LK1).


atualiza_ultimo_facto:-
    findall(N,facto(N,_),LNumeros),
    max_list(LNumeros, Max),
    retract(ultimo_facto(_)),
    assert(ultimo_facto(Max)).
