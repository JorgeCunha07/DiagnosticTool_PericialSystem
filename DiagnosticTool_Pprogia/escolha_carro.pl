:- encoding(utf8).

% Predicado para listar opções com índice numérico
listar_opcoes_numeradas([], _).
listar_opcoes_numeradas([Opcao|Resto], Index) :-
    write(Index), write('. '), write(Opcao), nl,
    NextIndex is Index + 1,
    listar_opcoes_numeradas(Resto, NextIndex).

% Predicado para listar todas as marcas disponíveis com índice numérico
listar_marcas :-
    findall(Marca, carro(_, Marca, _, _), Marcas),
    sort(Marcas, MarcasUnicas),
    write('Marcas disponíveis: '), nl,
    listar_opcoes_numeradas(MarcasUnicas, 1).
	
% Predicado para listar todas as marcas disponíveis sem índice numérico
listar_marcas(MarcasUnicas) :-
    findall(Marca, carro(_, Marca, _, _), Marcas),
    sort(Marcas, MarcasUnicas).

% Predicado para listar todos os modelos disponíveis para uma marca específica com índice numérico
listar_modelos(Marca) :-
    findall(Modelo, carro(_, Marca, Modelo, _), Modelos),
    sort(Modelos, ModelosUnicos),
    write('Modelos disponíveis para a marca '), write(Marca), write(': '), nl,
    listar_opcoes_numeradas(ModelosUnicos, 1).

% Predicado para listar todos os modelos disponíveis para uma marca específica sem índice numérico
listar_modelos(Marca, ModelosUnicos) :-
    findall(Modelo, carro(_, Marca, Modelo, _), Modelos),
    sort(Modelos, ModelosUnicos).

% Predicado para listar todos os motores disponíveis para uma marca e modelo específicos com índice numérico
listar_motores(Marca, Modelo) :-
    findall(Motor, carro(_, Marca, Modelo, Motor), Motores),
    sort(Motores, MotoresUnicos),
    write('Motores disponíveis para o modelo '), write(Modelo), write(' da marca '), write(Marca), write(': '), nl,
    listar_opcoes_numeradas(MotoresUnicos, 1).

% Predicado para listar todos os motores disponíveis para uma marca e modelo específicos sem índice numérico
listar_motores(Marca, Modelo, MotoresUnicos) :-
    findall(Motor, carro(_, Marca, Modelo, Motor), Motores),
    sort(Motores, MotoresUnicos).

% Predicado para obter a opção selecionada pelo índice
obter_opcao_por_indice(Lista, Indice, Opcao) :-
    nth1(Indice, Lista, Opcao).

% Predicado para perguntar ao utilizador e criar o primeiro teste
obter_numero_carro :-
    listar_marcas,
    write('Escolha o número da marca: '), nl,
    read(IndiceMarca),
    findall(Marca, carro(_, Marca, _, _), Marcas),
    sort(Marcas, MarcasUnicas),
    obter_opcao_por_indice(MarcasUnicas, IndiceMarca, Marca),
    
    listar_modelos(Marca),
    write('Escolha o número do modelo: '), nl,
    read(IndiceModelo),
    findall(Modelo, carro(_, Marca, Modelo, _), Modelos),
    sort(Modelos, ModelosUnicos),
    obter_opcao_por_indice(ModelosUnicos, IndiceModelo, Modelo),
    
    listar_motores(Marca, Modelo),
    write('Escolha o número do motor: '), nl,
    read(IndiceMotor),
    findall(Motor, carro(_, Marca, Modelo, Motor), Motores),
    sort(Motores, MotoresUnicos),
    obter_opcao_por_indice(MotoresUnicos, IndiceMotor, Motor),
    
    carro(Numero, Marca, Modelo, Motor),
    write('O número do carro é: '), write(Numero), nl,
    format(atom(Carro), '~w ~w ~w', [Marca, Modelo, Motor]),
    write('O carro selecionado foi: '), write(Carro), nl,
    
	retractall(carro_selecionado(_)),
	retractall(carro_numero_selecionado(_)),
	retractall(facto(_, _)),        % Reinicia os factos com o primeiro teste
	retractall(ultimo_facto(_)),
    retractall(justifica(_,_,_)), 	% Apaga todas as justificações para o como
	
	assertz(carro_selecionado(Carro)),
	assertz(carro_numero_selecionado(Numero)),
    assertz(ultimo_facto(0)),
    
    cria_facto(proximo_teste(Numero, problemas), 0, 0).
    
% Predicado para definir o carro do utilizador e criar o primeiro teste
obter_numero_carro(Numero, Carro) :-
	procurar_carro(Numero, Carro),
	
	retractall(carro_selecionado(_)),
	retractall(carro_numero_selecionado(_)),
    retractall(facto(_, _)),
	retractall(ultimo_facto(_)),
	retractall(justifica(_,_,_)),
	retractall(factos_processados(_)),
	
	assertz(carro_selecionado(Carro)),
	assertz(carro_numero_selecionado(Numero)),
	assertz(ultimo_facto(0)),
	
    cria_facto_server(proximo_teste(Numero, problemas), 0, 0).


procurar_carro(Numero, Carro) :-
    carro(Numero, Marca, Modelo, Motor),
    format(atom(Carro), '~w ~w ~w', [Marca, Modelo, Motor]).