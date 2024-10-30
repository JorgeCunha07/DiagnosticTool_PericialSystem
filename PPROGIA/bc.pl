:- encoding(utf8).
:-dynamic facto/2,ultimo_facto/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Metaconhecimento

facto_dispara_regras(problemas(_, sim), [1]).
facto_dispara_regras(problemas(_, nao), [2]).

facto_dispara_regras(liga(_, sim), [12]).
facto_dispara_regras(liga(_, nao), [11]).

facto_dispara_regras(bateria(_, _), [21, 22]).

facto_dispara_regras(vai_abaixo(_, sim), [23]).
facto_dispara_regras(vai_abaixo(_, nao), [24]).

facto_dispara_regras(motor_arranque_defeito(_, sim), [31]).
facto_dispara_regras(motor_arranque_defeito(_, nao), [32]).

facto_dispara_regras(motor_sobreaquece(_, sim), [33]).
facto_dispara_regras(motor_sobreaquece(_, nao), [34]).

facto_dispara_regras(barulho_anormal(_, sim), [35]).
facto_dispara_regras(barulho_anormal(_, nao), [36]).

facto_dispara_regras(ignicao_falha(_, sim), [41]).
facto_dispara_regras(ignicao_falha(_, nao), [42]).

facto_dispara_regras(radiador_ventoinha_defeito(_, sim), [43]).
facto_dispara_regras(radiador_ventoinha_defeito(_, nao), [44]).

facto_dispara_regras(falta_combustivel_ou_bomba_defeito(_, sim), [45]).
facto_dispara_regras(falta_combustivel_ou_bomba_defeito(_, nao), [46]).

facto_dispara_regras(barulho_no_motor(_, sim), [47]).
facto_dispara_regras(barulho_no_motor(_, nao), [48]).

facto_dispara_regras(dificuldade_travar(_, sim), [49]).
facto_dispara_regras(dificuldade_travar(_, nao), [410]).

facto_dispara_regras(sistema_seguranca_ativado(_, sim), [51]).
facto_dispara_regras(sistema_seguranca_ativado(_, nao), [52]).

facto_dispara_regras(fuga_arrefecimento(_, sim), [53]).
facto_dispara_regras(fuga_arrefecimento(_, nao), [54]).

facto_dispara_regras(filtro_combustivel_entupido(_, sim), [55]).
facto_dispara_regras(filtro_combustivel_entupido(_, nao), [56]).

facto_dispara_regras(correia_dentada_gasta(_, sim), [57]).
facto_dispara_regras(correia_dentada_gasta(_, nao), [58]).

facto_dispara_regras(barulho_suspensao_rodas(_, sim), [59]).
facto_dispara_regras(barulho_suspensao_rodas(_, nao), [510]).

facto_dispara_regras(pastilhas_travao_gastas(_, sim), [511]).
facto_dispara_regras(pastilhas_travao_gastas(_, nao), [512]).

facto_dispara_regras(problemas_caixa_velocidades(_, sim), [513]).
facto_dispara_regras(problemas_caixa_velocidades(_, nao), [514]).

facto_dispara_regras(fios_danificados(_, sim), [61]).
facto_dispara_regras(fios_danificados(_, nao), [62]).

facto_dispara_regras(termostato_defeituoso(_, sim), [63]).
facto_dispara_regras(termostato_defeituoso(_, nao), [64]).

facto_dispara_regras(injecao_combustivel_defeito(_, sim), [65]).
facto_dispara_regras(injecao_combustivel_defeito(_, nao), [66]).

facto_dispara_regras(problema_pneus_jantes_calcos(_, sim), [67]).
facto_dispara_regras(problema_pneus_jantes_calcos(_, nao), [68]).

facto_dispara_regras(discos_travao_gastos(_, sim), [69]).
facto_dispara_regras(discos_travao_gastos(_, nao), [610]).

facto_dispara_regras(amortecedores_gastos(_, sim), [76]).
facto_dispara_regras(amortecedores_gastos(_, nao), [77]).

facto_dispara_regras(fluido_transmissao(_, _), [78,79,710]).

facto_dispara_regras(bomba_agua_defeituosa(_, sim), [81]).
facto_dispara_regras(bomba_agua_defeituosa(_, nao), [821,822]).

facto_dispara_regras(velas_ignicao_defeito(_, sim), [83]).
facto_dispara_regras(velas_ignicao_defeito(_, nao), [84]).

facto_dispara_regras(fluido_travao(_, _), [85,86,87]).

facto_dispara_regras(embraiagem_gasta(_, sim), [88]).
facto_dispara_regras(embraiagem_gasta(_, nao), [89]).

facto_dispara_regras(sensores_defeituosos(_, sim), [91]).
facto_dispara_regras(sensores_defeituosos(_, nao), [92]).

facto_dispara_regras(servofreio(_, sim), [96]).
facto_dispara_regras(servofreio(_, nao), [97]).

facto_dispara_regras(sistema_ABS(_, sim), [103]).
facto_dispara_regras(sistema_ABS(_, nao), [104]).

facto_dispara_regras(alternador_defeito(_, sim), [72]).
facto_dispara_regras(alternador_defeito(_, nao), [721]).

facto_dispara_regras(compressor_AC(_, sim), [73]).
facto_dispara_regras(compressor_AC(_, nao), [731]).

facto_dispara_regras(tensores(_, sim), [74]).
facto_dispara_regras(tensores(_, nao), [75]).

facto_dispara_regras(oleo_motor(_, _), [93, 94, 95]).

facto_dispara_regras(sistema_escape(_, sim), [101]).
facto_dispara_regras(sistema_escape(_, nao), [102]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Factos perguntaveis
perguntavel(problemas(_, _)).
perguntavel(liga(_, _)).
perguntavel(bateria(_, _)).
perguntavel(motor_arranque_defeito(_, _)).
perguntavel(vai_abaixo(_, _)).
perguntavel(ignicao_falha(_, _)).
perguntavel(sistema_seguranca_ativado(_, _)).
perguntavel(fios_danificados(_, _)).
perguntavel(motor_sobreaquece(_, _)).
perguntavel(radiador_ventoinha_defeito(_, _)).
perguntavel(fuga_arrefecimento(_, _)).
perguntavel(termostato_defeituoso(_, _)).
perguntavel(bomba_agua_defeituosa(_, _)).
perguntavel(falta_combustivel_ou_bomba_defeito(_, _)).
perguntavel(filtro_combustivel_entupido(_, _)).
perguntavel(injecao_combustivel_defeito(_, _)).
perguntavel(velas_ignicao_defeito(_, _)).
perguntavel(sensores_defeituosos(_, _)).
perguntavel(barulho_anormal(_, _)).
perguntavel(barulho_no_motor(_, _)).
perguntavel(correia_dentada_gasta(_, _)).
perguntavel(amortecedores_gastos(_, _)).
perguntavel(dificuldade_travar(_, _)).
perguntavel(pastilhas_travao_gastas(_, _)).
perguntavel(discos_travao_gastos(_, _)).
perguntavel(fluido_travao(_, _)).
perguntavel(problemas_caixa_velocidades(_, _)).
perguntavel(fluido_transmissao(_, _)).
perguntavel(embraiagem_gasta(_, _)).
perguntavel(barulho_suspensao_rodas(_, _)).
perguntavel(problema_pneus_jantes_calcos(_, _)).
perguntavel(servofreio(_, _)).
perguntavel(sistema_ABS(_, _)).
perguntavel(alternador_defeito(_, _)).
perguntavel(compressor_AC(_, _)).
perguntavel(tensores(_, _)).
perguntavel(oleo_motor(_, _)).
perguntavel(sistema_escape(_, _)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Mapeamento de perguntas
pergunta(FactoPergunta,Pergunta):-
    carro_selecionado(Carro),
    texto_pergunta(FactoPergunta, Pergunta, Carro).

texto_pergunta(problemas(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'O veiculo ~w tem algum problema?', [Carro]).
texto_pergunta(liga(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'O veiculo ~w liga?', [Carro]).
texto_pergunta(bateria(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'Qual a bateria do veiculo ~w?', [Carro]).
texto_pergunta(motor_arranque_defeito(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'Motor de arranque do veículo ~w está com defeito?', [Carro]).
texto_pergunta(ignicao_falha(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'A ignição do veículo ~w está falhando?', [Carro]).
texto_pergunta(sistema_seguranca_ativado(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'O sistema de segurança do veículo ~w está ativado?', [Carro]).
texto_pergunta(fios_danificados(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'Os fios ou cabos do veículo ~w estão danificados ou em curto-circuito?', [Carro]).
texto_pergunta(motor_sobreaquece(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'O motor do veículo ~w está sobreaquecendo?', [Carro]).
texto_pergunta(radiador_ventoinha_defeito(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'O radiador ou a ventoinha do veículo ~w está com defeito?', [Carro]).
texto_pergunta(fuga_arrefecimento(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'Há alguma fuga no sistema de arrefecimento do veículo ~w?', [Carro]).
texto_pergunta(termostato_defeituoso(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'O termostato do veículo ~w está com defeito?', [Carro]).
texto_pergunta(bomba_agua_defeituosa(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'A bomba de água do veículo ~w está com defeito?', [Carro]).
texto_pergunta(falta_combustivel_ou_bomba_defeito(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'O veículo ~w está sem combustível ou a bomba de combustível está com defeito?', [Carro]).
texto_pergunta(filtro_combustivel_entupido(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'O filtro de combustível do veículo ~w está entupido?', [Carro]).
texto_pergunta(injecao_combustivel_defeito(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'O sistema de injeção de combustível do veículo ~w está com defeito?', [Carro]).
texto_pergunta(velas_ignicao_defeito(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'As velas de ignição do veículo ~w estão com defeito?', [Carro]).
texto_pergunta(sensores_defeituosos(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'Algum sensor do motor do veículo ~w está com defeito?', [Carro]).
texto_pergunta(vai_abaixo(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'O veículo ~w vai abaixo depois de ligar?', [Carro]).
texto_pergunta(barulho_anormal(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'O veículo ~w faz barulho anormal?', [Carro]).
texto_pergunta(barulho_no_motor(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'O barulho do veículo ~w vem do motor?', [Carro]).
texto_pergunta(correia_dentada_gasta(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'A correia dentada do veiculo ~w esta gasta?', [Carro]).
texto_pergunta(barulho_suspensao_rodas(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'O barulho na suspensão ou rodas do veiculo ~w faz barulho?', [Carro]).
texto_pergunta(problema_pneus_jantes_calcos(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'O veiculo ~w tem problema com pneus, jantes ou calços de travão?', [Carro]).
texto_pergunta(amortecedores_gastos(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'O veiculo ~w tem amortecedores gastos ou bases de amortecedores gastas?', [Carro]).
texto_pergunta(dificuldade_travar(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'O veiculo ~w apresenta dificuldade em travar?', [Carro]).
texto_pergunta(problemas_caixa_velocidades(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'O veiculo ~w apresenta problemas na caixa de velocidade?', [Carro]).
texto_pergunta(pastilhas_travao_gastas(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'O veiculo ~w apresenta as pastilhas de travão gastas?', [Carro]).
texto_pergunta(discos_travao_gastos(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'O veiculo ~w apresenta discos de travão gastos?', [Carro]).
texto_pergunta(fluido_transmissao(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'Qual é o nivel do fluido da caixa de velocidade que o veiculo ~w apresenta?', [Carro]).
texto_pergunta(embraiagem_gasta(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'A embraiagem do veiculo ~w apresenta sinais de desgaste ou de patinar?', [Carro]).
texto_pergunta(fluido_travao(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'Qual é o nivel do fluido dos travoes do veiculo ~w?', [Carro]).
texto_pergunta(servofreio(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'O veiculo ~w tem problema no servofreio?', [Carro]).
texto_pergunta(sistema_ABS(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'O veiculo ~w tem problema no sistema de ABS?', [Carro]).
texto_pergunta(alternador_defeito(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'O alternador do veiculo ~w tem defeito?', [Carro]).
texto_pergunta(compressor_AC(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'O compressor de AC do veiculo ~w tem defeito?', [Carro]).
texto_pergunta(tensores(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'Os tensores do veiculo ~w tem defeito?', [Carro]).
texto_pergunta(oleo_motor(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), 'Qual e o nivel de oleo do motor do veiculo ~w?', [Carro]).
texto_pergunta(sistema_escape(_, _), Pergunta, Carro) :-
    format(atom(Pergunta), '0 veiculo ~w tem problema no sistema de escape?', [Carro]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Define as opções válidas para as respostas
opcoes_validas(problemas(_, _), [sim, nao]).
opcoes_validas(liga(_, _), [sim, nao]).
opcoes_validas(bateria(_, _), [0, Max]):-carro_numero_selecionado(Carro),bateria(Carro, _, _, Max).
opcoes_validas(motor_arranque_defeito(_, _), [sim, nao]).
opcoes_validas(ignicao_falha(_, _), [sim, nao]).
opcoes_validas(sistema_seguranca_ativado(_, _), [sim, nao]).
opcoes_validas(fios_danificados(_, _), [sim, nao]).
opcoes_validas(vai_abaixo(_, _), [sim, nao]).
opcoes_validas(motor_sobreaquece(_, _), [sim, nao]).
opcoes_validas(radiador_ventoinha_defeito(_, _), [sim, nao]).
opcoes_validas(fuga_arrefecimento(_, _), [sim, nao]).
opcoes_validas(termostato_defeituoso(_, _), [sim, nao]).
opcoes_validas(bomba_agua_defeituosa(_, _), [sim, nao]).
opcoes_validas(falta_combustivel_ou_bomba_defeito(_, _), [sim, nao]).
opcoes_validas(filtro_combustivel_entupido(_, _), [sim, nao]).
opcoes_validas(injecao_combustivel_defeito(_, _), [sim, nao]).
opcoes_validas(velas_ignicao_defeito(_, _), [sim, nao]).
opcoes_validas(sensores_defeituosos(_, _), [sim, nao]).
opcoes_validas(barulho_anormal(_, _), [sim, nao]).
opcoes_validas(barulho_no_motor(_, _), [sim, nao]).
opcoes_validas(correia_dentada_gasta(_, _), [sim, nao]).
opcoes_validas(amortecedores_gastos(_, _), [sim, nao]).
opcoes_validas(dificuldade_travar(_, _), [sim, nao]).
opcoes_validas(pastilhas_travao_gastas(_, _), [sim, nao]).
opcoes_validas(discos_travao_gastos(_, _), [sim, nao]).
opcoes_validas(fluido_travao(_, _), [0, Max]):-carro_numero_selecionado(Carro),fluido_travao(Carro, _, _, Max).
opcoes_validas(problemas_caixa_velocidades(_, _), [sim, nao]).
opcoes_validas(fluido_transmissao(_, _), [0, Max]):-carro_numero_selecionado(Carro),fluido_transmissao(Carro, _, _, Max).
opcoes_validas(embraiagem_gasta(_, _), [sim, nao]).
opcoes_validas(barulho_suspensao_rodas(_, _), [sim, nao]).
opcoes_validas(problema_pneus_jantes_calcos(_, _), [sim, nao]).
opcoes_validas(servofreio(_, _), [sim, nao]).
opcoes_validas(sistema_ABS(_, _), [sim, nao]).
opcoes_validas(alternador_defeito(_, _), [sim, nao]).
opcoes_validas(compressor_AC(_, _), [sim, nao]).
opcoes_validas(tensores(_, _), [sim, nao]).
opcoes_validas(oleo_motor(_, _), [0, Max]):-carro_numero_selecionado(Carro),oleo_motor(Carro, _, _, Max).
opcoes_validas(sistema_escape(_, _), [0, Max]):-carro_numero_selecionado(Carro),sistema_escape(Carro, _, _, Max).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ultimo_facto(1).
% ultima_regra(8).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% As regras estão relacionadas com o diagrama 'FlowChartDiagnostico_final_regras.puml' 
% adjacente à solução proposta.
% As regras estão organizadas por nível de profundidade da árvore, para ajudar 
% a uma melhor identificação das mesmas no fluxograma.
% Ex. O nível de profundidade 5 contém as regras 51, 52, ..., 510, etc.

% Se o carro NAO apresenta algum problema então não tem problemas
regra 1
	se [problemas(Veiculo,sim)]
	entao [cria_facto(proximo_teste(Veiculo, liga))].
	
% Se o carro NAO liga vamos verificar a bateria (Verificar a bateria e cabos de bateria)
regra 11
	se [liga(Veiculo,nao)]
	entao [cria_facto(proximo_teste(Veiculo, bateria))].
	
% Se o carro apresenta algum problema então temos de verificar Se o carro liga
regra 2
	se [problemas(Veiculo,nao)]
	entao [cria_facto(diagnostico(Veiculo,'Nenhum problema identificado.')), cria_facto(solucao(Veiculo,'Nao ha necessidade de apresentar solucao.'))].

% Se o carro liga vamos verificar se o carro vai abaixo
regra 12
	se [liga(Veiculo,sim)]
	entao [cria_facto(proximo_teste(Veiculo, vai_abaixo))].
	
% Se o carro está com bateria fraca ou sem carga
regra 21
    se [avalia(bateria(Veiculo,<,'Min'))]
	entao [cria_facto(diagnostico(Veiculo,'Bateria com defeito ou sem carga.')), cria_facto(solucao(Veiculo,'Substituir ou recarregar a bateria.'))].

% Se o carro NAO esta com bateria fraca ou sem carga
regra 22
    se [avalia(bateria(Veiculo,>=,'Min'))]
	entao [cria_facto(proximo_teste(Veiculo, motor_arranque_defeito))].

% Se o carro vai abaixo
regra 23
    se [vai_abaixo(Veiculo, sim)]
    entao [cria_facto(proximo_teste(Veiculo, motor_sobreaquece))].

% Se o carro NAO vai abaixo
regra 24
    se [vai_abaixo(Veiculo, nao)]
    entao [cria_facto(proximo_teste(Veiculo, barulho_anormal))].

% Se o motor de arranque esta com defeito
regra 31
    se [motor_arranque_defeito(Veiculo, sim)]
    entao [cria_facto(diagnostico(Veiculo, 'Falha no motor de arranque.')), cria_facto(solucao(Veiculo,'Substituir motor de arranque.'))].

% Se o motor de arranque NAO esta com defeito
regra 32
    se [motor_arranque_defeito(Veiculo, nao)]
    entao [cria_facto(proximo_teste(Veiculo, ignicao_falha))].

% Se o motor sobreaquece
regra 33
    se [motor_sobreaquece(Veiculo, sim)]
    entao [cria_facto(proximo_teste(Veiculo, radiador_ventoinha_defeito))].

% Se o motor NAO sobreaquece
regra 34
    se [motor_sobreaquece(Veiculo, nao)]
    entao [cria_facto(proximo_teste(Veiculo, falta_combustivel_ou_bomba_defeito))].

% Se o carro faz barulho anormal
regra 35
    se [barulho_anormal(Veiculo, sim)]
    entao [cria_facto(proximo_teste(Veiculo, barulho_no_motor))].

% Se o carro NAO faz barulho anormal
regra 36
    se [barulho_anormal(Veiculo, nao)]
    entao [cria_facto(proximo_teste(Veiculo, dificuldade_travar))].
	
% Se a ignicao esta a falhar
regra 41
    se [ignicao_falha(Veiculo, sim)]
    entao [cria_facto(diagnostico(Veiculo, 'Sistema de ignicao com defeito.')), cria_facto(solucao(Veiculo,'Verificar chave de ignicao, fusiveis, conectores e verificacao de alimentacao de combustivel (pressao de combustivel).'))].

% Se a ignicao NAO esta a falhar
regra 42
    se [ignicao_falha(Veiculo, nao)]
    entao [cria_facto(proximo_teste(Veiculo, sistema_seguranca_ativado))].

% Se o radiador ou ventoinha esta com defeito
regra 43
    se [radiador_ventoinha_defeito(Veiculo, sim)]
    entao [cria_facto(diagnostico(Veiculo, 'Problema no radiador ou ventoinha.')), cria_facto(solucao(Veiculo, 'Substituir radiador/ventoinha.'))].

% Se o radiador ou ventoinha NAO esta com defeito
regra 44
    se [radiador_ventoinha_defeito(Veiculo, nao)]
    entao [cria_facto(proximo_teste(Veiculo, fuga_arrefecimento))].

% Se o carro esta sem combustivel ou a bomba de combustivel esta com defeito
regra 45
    se [falta_combustivel_ou_bomba_defeito(Veiculo, sim)]
    entao [cria_facto(diagnostico(Veiculo, 'Falta de combustivel ou bomba de combustivel com defeito')), cria_facto(solucao(Veiculo, 'Reabastecer ou substituir a bomba de combustivel.'))].

% Se o carro NAO esta sem combustivel ou a bomba de combustivel NAO esta com defeito
regra 46
    se [falta_combustivel_ou_bomba_defeito(Veiculo, nao)]
    entao [cria_facto(proximo_teste(Veiculo, filtro_combustivel_entupido))].

% Se o carro faz barulho no motor
regra 47
    se [barulho_no_motor(Veiculo, sim)]
    entao [cria_facto(proximo_teste(Veiculo, correia_dentada_gasta))].

% Se o carro NAO faz barulho no motor
regra 48
    se [barulho_no_motor(Veiculo, nao)]
    entao [cria_facto(proximo_teste(Veiculo, barulho_suspensao_rodas))].

% Se o carro apresenta dificuldade em travar
regra 49
    se [dificuldade_travar(Veiculo, sim)]
    entao [cria_facto(proximo_teste(Veiculo, pastilhas_travao_gastas))].

% Se o carro NAO apresenta dificuldade em travar
regra 410
    se [dificuldade_travar(Veiculo, nao)]
    entao [cria_facto(proximo_teste(Veiculo, problemas_caixa_velocidades))].

% Se o sistema de seguranca esta ativado
regra 51
    se [sistema_seguranca_ativado(Veiculo, sim)]
    entao [cria_facto(diagnostico(Veiculo, 'Sistema de segurança ativado.')), cria_facto(solucao(Veiculo, 'Desactivar sistema de seguranca.'))].

% Se o sistema de seguranca NAO esta ativado
regra 52
    se [sistema_seguranca_ativado(Veiculo, nao)]
    entao [cria_facto(proximo_teste(Veiculo, fios_danificados))].

% Se o sistema de arrefecimento esta com fuga
regra 53
    se [fuga_arrefecimento(Veiculo, sim)]
    entao [cria_facto(diagnostico(Veiculo, 'Fuga no sistema de arrefecimento.')), cria_facto(solucao(Veiculo, 'Reparar fuga e completar fluido.'))].

% Se o sistema de arrefecimento NAO esta com fuga
regra 54
    se [fuga_arrefecimento(Veiculo, nao)]
    entao [cria_facto(proximo_teste(Veiculo, termostato_defeituoso))].

% Se o filtro de combustivel esta entupido
regra 55
    se [filtro_combustivel_entupido(Veiculo, sim)]
    entao [cria_facto(diagnostico(Veiculo, 'Filtro de combustível entupido.')), cria_facto(solucao(Veiculo, 'Substituir filtro de combustivel.'))].

% Se o filtro de combustivel NAO esta entupido
regra 56
    se [filtro_combustivel_entupido(Veiculo, nao)]
    entao [cria_facto(proximo_teste(Veiculo, injecao_combustivel_defeito))].

% Se a correia dentada esta gasta
regra 57
    se [correia_dentada_gasta(Veiculo, sim)]
    entao [cria_facto(diagnostico(Veiculo, 'Correia dentada gasta.')), cria_facto(solucao(Veiculo, 'Substituir kit de distribuicao.'))].

% Se a correia dentada NAO esta gasta
regra 58
    se [correia_dentada_gasta(Veiculo, nao)]
    entao [cria_facto(proximo_teste(Veiculo, bomba_agua_defeituosa))].

% Se o carro faz barulho na suspensao ou rodas
regra 59
    se [barulho_suspensao_rodas(Veiculo, sim)]
    entao [cria_facto(proximo_teste(Veiculo, problema_pneus_jantes_calcos))].

% Se o carro NAO faz barulho na suspensao ou rodas
regra 510
    se [barulho_suspensao_rodas(Veiculo, nao)]
    entao [cria_facto(diagnostico(Veiculo, 'Sem barulhos anormais.')), cria_facto(solucao(Veiculo, 'Nao ha necessidade de apresentar solucao'))].

% Se as pastilhas de travao estão gastas
regra 511
    se [pastilhas_travao_gastas(Veiculo, sim)]
    entao [cria_facto(diagnostico(Veiculo, 'Pastilhas de travao gastas.')), cria_facto(solucao(Veiculo, 'Substituir pastilhas de travao.'))].

% Se as pastilhas de travao NAO estão gastas
regra 512
    se [pastilhas_travao_gastas(Veiculo, nao)]
    entao [cria_facto(proximo_teste(Veiculo, discos_travao_gastos))].

% Se o carro apresenta problemas na caixa de velocidades
regra 513	
    se [problemas_caixa_velocidades(Veiculo, sim)]
	entao [cria_facto(proximo_teste(Veiculo, fluido_transmissao))].

% Se o carro NAO apresenta problemas na caixa de velocidades
regra 514
    se [problemas_caixa_velocidades(Veiculo, nao)]
    entao [cria_facto(diagnostico(Veiculo, 'Nenhum problema identificado.')), cria_facto(solucao(Veiculo, 'Nao ha necessidade de apresentar solucao.'))].

% Se os fios estao danificados
regra 61
    se [fios_danificados(Veiculo, sim)]
    entao [cria_facto(diagnostico(Veiculo, 'Fios com corte ou curto-circuito.')), cria_facto(solucao(Veiculo, 'Reparar ou substituir cabo condutor de energia eletrica.'))].

% Se os fios estao danificados
regra 62
    se [fios_danificados(Veiculo, nao)]
    entao [cria_facto(diagnostico(Veiculo, 'Verificação adicional necessaria.')), cria_facto(solucao(Veiculo, 'Seguir protocolo de reparacao da marca, consultar especialista.'))].

% Se o termostato esta com defeito
regra 63
    se [termostato_defeituoso(Veiculo, sim)]
    entao [cria_facto(diagnostico(Veiculo, 'Termostato com defeito.')), cria_facto(solucao(Veiculo, 'Substituir termostato.'))].

% Se o termostato NAO esta com defeito
regra 64
    se [termostato_defeituoso(Veiculo, nao)]
    entao [cria_facto(proximo_teste(Veiculo, bomba_agua_defeituosa))].

% Se o sistema de injecao de combustivel esta com defeito
regra 65
    se [injecao_combustivel_defeito(Veiculo, sim)]
    entao [cria_facto(diagnostico(Veiculo, 'Injeção de combustivel com defeito.')), cria_facto(solucao(Veiculo, 'Limpar ou substituir injetores.'))].

% Se o sistema de injecao de combustivel NAO esta com defeito
regra 66
    se [injecao_combustivel_defeito(Veiculo, nao)]
    entao [cria_facto(proximo_teste(Veiculo, velas_ignicao_defeito))].

% Se o carro apresenta problemas nos pneus, jantes ou calcos de travão
regra 67
    se [problema_pneus_jantes_calcos(Veiculo, sim)]
    entao [cria_facto(diagnostico(Veiculo, 'Problema nos pneus, jantes ou calcos de travao.')), cria_facto(solucao(Veiculo, 'Reparar ou substituir pneus, jantes ou calcos de travao conforme necessario.'))].

% Se o carro NAO apresenta problemas nos pneus, jantes ou calcos de travão
regra 68
    se [problema_pneus_jantes_calcos(Veiculo, nao)]
    entao [cria_facto(proximo_teste(Veiculo, amortecedores_gastos))].

% Se os discos de travão estão gastos
regra 69
    se [discos_travao_gastos(Veiculo, sim)]
    entao [cria_facto(diagnostico(Veiculo, 'Discos de travão gastos.')), cria_facto(solucao(Veiculo, 'Substituir discos de travao.'))].

% Se os discos de travão NAO estão gastos
regra 610
    se [discos_travao_gastos(Veiculo, nao)]
    entao [cria_facto(proximo_teste(Veiculo, fluido_travao))].

% Se o sistema de ABS esta com defeito
regra 72
    se [alternador_defeito(Veiculo, sim)]
    entao [cria_facto(diagnostico(Veiculo, 'Alternador com defeito.')), cria_facto(solucao(Veiculo, 'Substituir alternador.'))].

% Se o sistema de ABS NAO esta com defeito
regra 721
    se [alternador_defeito(Veiculo, nao)]
    entao [cria_facto(proximo_teste(Veiculo, compressor_AC))].

% Se o compressor de AC esta com defeito
regra 73
    se [compressor_AC(Veiculo, sim)]
    entao [cria_facto(diagnostico(Veiculo, 'Compressor de AC com defeito.')), cria_facto(solucao(Veiculo, 'Substituir compressor de ar condicionado.'))].

% Se o compressor de AC NAO esta com defeito
regra 731
    se [compressor_AC(Veiculo, nao)]
    entao [cria_facto(proximo_teste(Veiculo, tensores))].

% Se os tensores estao com defeito
regra 74
    se [tensores(Veiculo, sim)]
    entao [cria_facto(diagnostico(Veiculo, 'Falha nos tensores da correia.')), cria_facto(solucao(Veiculo, 'Substituir tensores da correia.'))].

% Se os tensores NAO estao com defeito
regra 75
    se [tensores(Veiculo, nao)]
    entao [cria_facto(proximo_teste(Veiculo, oleo_motor))].

% Se os amortecedores estao gastos
regra 76
    se [amortecedores_gastos(Veiculo, sim)]
    entao [cria_facto(diagnostico(Veiculo, 'Amortecedores gastos.')), cria_facto(solucao(Veiculo, 'Substituir kit de amortecedores.'))].

% Se os amortecedores NAO estao gastos
regra 77
    se [amortecedores_gastos(Veiculo, nao)]
    entao [cria_facto(diagnostico(Veiculo, 'Problema nao identificado.')), cria_facto(solucao(Veiculo, 'Consultar especialista.'))].

% Se o nivel do fluido da caixa de velocidade esta fora dos limites
regra 78
    se [avalia(fluido_transmissao(Veiculo,<,'Min'))]
	entao [cria_facto(diagnostico(Veiculo,'Nível de fluido de transmissao baixo.')), cria_facto(solucao(Veiculo, 'Completar fluido de transmissao e verificar possiveis fugas.'))].

% Se o nivel do fluido da caixa de velocidade esta fora dos limites
regra 79
    se [avalia(fluido_transmissao(Veiculo,>,'Max'))]
	entao [cria_facto(diagnostico(Veiculo,'Excesso de fluido de transmissao.')), cria_facto(solucao(Veiculo, 'Ajustar fluido de transmissao.'))].

% Se o nivel do fluido da caixa de velocidade esta dentro dos limites
regra 710
    se [avalia(fluido_transmissao(Veiculo,>,'Min')) e avalia(fluido_transmissao(Veiculo,<,'Max'))]
	entao [cria_facto(proximo_teste(Veiculo, embraiagem_gasta))].

% Se a bomba de agua esta com defeito
regra 81
    se [bomba_agua_defeituosa(Veiculo, sim)]
    entao [cria_facto(diagnostico(Veiculo, 'Falha na bomba de agua.')), cria_facto(solucao(Veiculo, 'Substituir bomba de agua.'))].

% Se a bomba de agua NAO esta com defeito e a correia dentada NAO esta gasta
regra 821
    se [bomba_agua_defeituosa(Veiculo, nao) e correia_dentada_gasta(Veiculo,nao)]
    entao [cria_facto(proximo_teste(Veiculo, alternador_defeito))].

% Se a bomba de agua NAO esta com defeito e o termoestato NAO esta com defeito
regra 822
    se [bomba_agua_defeituosa(Veiculo, nao) e termostato_defeituoso(_, nao)]
    entao [cria_facto(diagnostico(Veiculo, 'Verificação adicional necessaria.')), cria_facto(solucao(Veiculo, 'Consultar especialista.'))].

% Se as velas de ignicao estao com defeito
regra 83
    se [velas_ignicao_defeito(Veiculo, sim)]
    entao [cria_facto(diagnostico(Veiculo, 'Falha nas velas de ignição')), cria_facto(solucao(Veiculo, 'Substituir velas de ignicao.'))].

% Se as velas de ignicao NAO estao com defeito
regra 84
    se [velas_ignicao_defeito(Veiculo, nao)]
    entao [cria_facto(proximo_teste(Veiculo, sensores_defeituosos))].

% Se o nivel do fluido dos travoes esta fora dos limites
regra 85
    se [avalia(fluido_travao(Veiculo,<,'Min'))]
	entao [cria_facto(diagnostico(Veiculo,'Nivel de fluido insuficiente.')), cria_facto(solucao(Veiculo, 'Completar fluido de travão e corrigir possiveis fugas.'))].

% Se o nivel do fluido dos travoes esta fora dos limites
regra 86
    se [avalia(fluido_travao(Veiculo,>,'Max'))]
	entao [cria_facto(diagnostico(Veiculo,'Excesso de fluido de travao.')), cria_facto(solucao(Veiculo, 'Ajustar nivel de fluido de travao.'))].

% Se o nivel do fluido dos travoes esta dentro dos limites
regra 87
    se [avalia(fluido_travao(Veiculo,>,'Min')) e avalia(fluido_travao(Veiculo,<,'Max'))]
	entao [cria_facto(proximo_teste(Veiculo, servofreio))].

% Se a embraiagem esta gasta
regra 88
    se [embraiagem_gasta(Veiculo, sim)]
    entao [cria_facto(diagnostico(Veiculo, 'Embraiagem com desgaste excessivo ou volante bimassa defeituoso.')), cria_facto(solucao(Veiculo, 'Substituir kit de embraiagem.'))].

% Se a embraiagem NAO esta gasta
regra 89
    se [embraiagem_gasta(Veiculo, nao)]
    entao [cria_facto(diagnostico(Veiculo, 'Verificacao adicional necessaria.')), cria_facto(solucao(Veiculo, 'Consultar especialista.'))].

% Se os sensores do motor estao com defeito
regra 91
	se [sensores_defeituosos(Veiculo, sim)]
    entao [cria_facto(diagnostico(Veiculo, 'Sensor do motor com falha.')), cria_facto(solucao(Veiculo, 'Verificar e substituir sensores.'))].

% Se os sensores do motor NAO estao com defeito
regra 92
	se [sensores_defeituosos(Veiculo, nao)]
    entao [cria_facto(diagnostico(Veiculo, 'Problema indeterminado.')), cria_facto(solucao(Veiculo, 'Consultar especialista.'))].

% Se o nivel de oleo do motor esta fora dos limites
regra 93
    se [avalia(oleo_motor(Veiculo,<,'Min'))]
	entao [cria_facto(diagnostico(Veiculo,'Nivel de oleo insuficiente.')), cria_facto(solucao(Veiculo, 'Completar nivel de oleo.'))].

% Se o nivel de oleo do motor esta fora dos limites
regra 94
    se [avalia(oleo_motor(Veiculo,>,'Max'))]
	entao [cria_facto(diagnostico(Veiculo,'Excesso de oleo.')), cria_facto(solucao(Veiculo, 'Retirar excesso de oleo.'))].

% Se o nivel de oleo do motor esta dentro dos limites
regra 95
    se [avalia(oleo_motor(Veiculo,>,'Min')) e avalia(oleo_motor(Veiculo,<,'Max'))]
	entao [cria_facto(proximo_teste(Veiculo, sistema_escape))].

% Se o servofreio esta com defeito
regra 96
	se [servofreio(Veiculo, sim)]
    entao [cria_facto(diagnostico(Veiculo, 'Servofreio defeituoso.')), cria_facto(solucao(Veiculo, 'Reparar ou substituir servofreio.'))].

% Se o servofreio NAO esta com defeito
regra 97
	se [servofreio(Veiculo, nao)]
    entao [cria_facto(proximo_teste(Veiculo, sistema_ABS))].

% Se o sistema de escape esta com defeito
regra 101
	se [sistema_escape(Veiculo, sim)]
    entao [cria_facto(diagnostico(Veiculo, 'Vazamento ou bloqueio no sistema de escape.')), cria_facto(solucao(Veiculo, 'Reparar ou substituir sistema de escape.'))].

% Se o sistema de escape NAO esta com defeito
regra 102
	se [sistema_escape(Veiculo, nao)]
    entao [cria_facto(diagnostico(Veiculo, 'Verificacao adicional necessaria.')), cria_facto(solucao(Veiculo, 'Consultar especialista.'))].

% Se o sistema de ABS esta com defeito
regra 103
	se [sistema_ABS(Veiculo, sim)]
    entao [cria_facto(diagnostico(Veiculo, 'Sistema ABS com defeito.')), cria_facto(solucao(Veiculo, 'Verificar e reparar sistema ABS com consulta de especialista.'))].

% Se o sistema de ABS NAO esta com defeito
regra 104
	se [sistema_ABS(Veiculo, nao)]
    entao [cria_facto(diagnostico(Veiculo, 'Problema indeterminado.')), cria_facto(solucao(Veiculo, 'Consultar especialista.'))].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Factos para carros
carro(1, 'Opel', 'Astra J', '1.4 Turbo').
carro(2, 'Opel', 'Astra J', '1.6 CDTi').
carro(3, 'Opel', 'Astra J', '2.0 CDTi').
carro(4, 'Opel', 'Corsa E', '1.2 Ecotec').
carro(5, 'Opel', 'Corsa E', '1.4 Ecotec').
carro(6, 'Opel', 'Insignia B', '1.5 Turbo').
carro(7, 'Opel', 'Insignia B', '2.0 CDTi').
carro(8, 'Opel', 'Crossland X', '1.2 Turbo').
carro(9, 'Opel', 'Crossland X', '1.5 Diesel').
carro(10, 'Opel', 'Grandland X', '1.2 Turbo').
carro(11, 'Opel', 'Grandland X', '1.6 Hybrid').
carro(12, 'Peugeot', '208 II', '1.2 PureTech').
carro(13, 'Peugeot', '208 II', '1.5 BlueHDi').
carro(14, 'Peugeot', '308 II', '1.2 PureTech').
carro(15, 'Peugeot', '308 II', '1.6 BlueHDi').
carro(16, 'Peugeot', '3008 II', '1.6 PureTech').
carro(17, 'Peugeot', '3008 II', '2.0 BlueHDi').
carro(18, 'Peugeot', '508 II', '1.6 PureTech').
carro(19, 'Peugeot', '508 II', '1.5 BlueHDi').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Factos para motor de arranque
motor_arranque(1, 1.2, 1.2, 3).
motor_arranque(2, 1.4, 1.4, 3).
motor_arranque(3, 1.6, 1.6, 3).
motor_arranque(4, 1.1, 1.1, 3).
motor_arranque(5, 1.3, 1.3, 3).
motor_arranque(6, 1.5, 1.5, 3).
motor_arranque(7, 1.7, 1.7, 3).
motor_arranque(8, 1.2, 1.2, 3).
motor_arranque(9, 1.4, 1.4, 3).
motor_arranque(10, 1.3, 1.3, 3).
motor_arranque(11, 1.5, 1.5, 3).
motor_arranque(12, 1.6, 1.6, 3).
motor_arranque(13, 1.7, 1.7, 3).
motor_arranque(14, 1.6, 1.6, 3).
motor_arranque(15, 1.7, 1.7, 3).
motor_arranque(16, 1.3, 1.3, 3).
motor_arranque(17, 1.4, 1.4, 3).
motor_arranque(18, 1.3, 1.3, 3).
motor_arranque(19, 1.6, 1.6, 3).

% Factos para bloco de motor
bloco_motor(1, 85, 105, 120).
bloco_motor(2, 85, 110, 120).
bloco_motor(3, 85, 110, 120).
bloco_motor(4, 85, 105, 120).
bloco_motor(5, 85, 110, 120).
bloco_motor(6, 85, 105, 120).
bloco_motor(7, 85, 110, 120).
bloco_motor(8, 85, 105, 120).
bloco_motor(9, 85, 110, 120).
bloco_motor(10, 85, 105, 120).
bloco_motor(11, 85, 110, 120).
bloco_motor(12, 85, 105, 120).
bloco_motor(13, 85, 110, 120).
bloco_motor(14, 85, 105, 120).
bloco_motor(15, 85, 110, 120).
bloco_motor(16, 85, 105, 120).
bloco_motor(17, 85, 110, 120).
bloco_motor(18, 85, 105, 120).
bloco_motor(19, 85, 110, 120).

% Factos para bateria
bateria(1, 60, 70, 70).
bateria(2, 65, 75, 75).
bateria(3, 70, 80, 80).
bateria(4, 50, 60, 60).
bateria(5, 55, 65, 65).
bateria(6, 70, 80, 80).
bateria(7, 75, 85, 85).
bateria(8, 50, 60, 60).
bateria(9, 55, 65, 65).
bateria(10, 60, 70, 70).
bateria(11, 65, 75, 75).
bateria(12, 70, 80, 80).
bateria(13, 75, 85, 85).
bateria(14, 70, 80, 80).
bateria(15, 75, 85, 85).
bateria(16, 60, 70, 70).
bateria(17, 65, 75, 75).
bateria(18, 70, 80, 80).
bateria(19, 70, 80, 80).

% Factos para líquido de arrefecimento
liquido_arrefecimento(1, 5.5, 7, 10).
liquido_arrefecimento(2, 6, 8, 10).
liquido_arrefecimento(3, 6.5, 9, 10).
liquido_arrefecimento(4, 5.5, 7, 10).
liquido_arrefecimento(5, 6, 8, 10).
liquido_arrefecimento(6, 7, 9, 10).
liquido_arrefecimento(7, 7.5, 9.5, 10).
liquido_arrefecimento(8, 6.5, 8, 10).
liquido_arrefecimento(9, 6.5, 8.5, 10).
liquido_arrefecimento(10, 6, 8, 10).
liquido_arrefecimento(11, 6.5, 8.5, 10).
liquido_arrefecimento(12, 7, 9, 10).
liquido_arrefecimento(13, 7.5, 9.5, 10).
liquido_arrefecimento(14, 7, 9, 10).
liquido_arrefecimento(15, 7.5, 9.5, 10).
liquido_arrefecimento(16, 6, 8, 10).
liquido_arrefecimento(17, 6.5, 8.5, 10).
liquido_arrefecimento(18, 6, 8, 10).
liquido_arrefecimento(19, 6.5, 8.5, 10).

% Factos para óleo do motor
oleo_motor(1, 4, 5, 8).
oleo_motor(2, 4.5, 5, 8).
oleo_motor(3, 5, 5.5, 8).
oleo_motor(4, 3.5, 4, 8).
oleo_motor(5, 4, 4.5, 8).
oleo_motor(6, 4.5, 5, 8).
oleo_motor(7, 5, 5.5, 8).
oleo_motor(8, 3.5, 4, 8).
oleo_motor(9, 4, 4.5, 8).
oleo_motor(10, 3.5, 4, 8).
oleo_motor(11, 4, 4.5, 8).
oleo_motor(12, 3.5, 4, 8).
oleo_motor(13, 4, 4.5, 8).
oleo_motor(14, 4, 4.5, 8).
oleo_motor(15, 4, 4.5, 8).
oleo_motor(16, 4.5, 5, 8).
oleo_motor(17, 5, 5.5, 8).
oleo_motor(18, 4.5, 5, 8).
oleo_motor(19, 5, 5.5, 8).

% Factos para fluido de travão
fluido_travao(1, 0.5, 1, 1.2).
fluido_travao(2, 0.6, 1, 1.2).
fluido_travao(3, 0.7, 1, 1.2).
fluido_travao(4, 0.5, 0.8, 1.2).
fluido_travao(5, 0.5, 0.9, 1.2).
fluido_travao(6, 0.7, 1, 1.2).
fluido_travao(7, 0.8, 1, 1.2).
fluido_travao(8, 0.5, 0.8, 1.2).
fluido_travao(9, 0.6, 0.9, 1.2).
fluido_travao(10, 0.6, 0.9, 1.2).
fluido_travao(11, 0.7, 1, 1.2).
fluido_travao(12, 0.7, 1, 1.2).
fluido_travao(13, 0.8, 1, 1.2).
fluido_travao(14, 0.7, 1, 1.2).
fluido_travao(15, 0.8, 1, 1.2).
fluido_travao(16, 0.6, 0.9, 1.2).
fluido_travao(17, 0.6, 1, 1.2).
fluido_travao(18, 0.6, 0.9, 1.2).
fluido_travao(19, 0.7, 1, 1.2).


% Factos para fluido de transmissão
fluido_transmissao(1, 1.8, 2.5, 3).
fluido_transmissao(2, 2, 2.5, 3).
fluido_transmissao(3, 2, 2.5, 3).
fluido_transmissao(4, 1.6, 2.3, 3).
fluido_transmissao(5, 1.8, 2.4, 3).
fluido_transmissao(6, 2, 2.5, 3).
fluido_transmissao(7, 2.2, 2.8, 3).
fluido_transmissao(8, 1.6, 2.2, 3).
fluido_transmissao(9, 1.8, 2.4, 3).
fluido_transmissao(10, 1.8, 2.3, 3).
fluido_transmissao(11, 2, 2.5, 3).
fluido_transmissao(12, 2, 2.5, 3).
fluido_transmissao(13, 2.2, 2.8, 3).
fluido_transmissao(14, 2, 2.5, 3).
fluido_transmissao(15, 2.2, 2.8, 3).
fluido_transmissao(16, 1.8, 2.5, 3).
fluido_transmissao(17, 2, 2.5, 3).
fluido_transmissao(18, 1.8, 2.5, 3).
fluido_transmissao(19, 2, 2.5, 3).

