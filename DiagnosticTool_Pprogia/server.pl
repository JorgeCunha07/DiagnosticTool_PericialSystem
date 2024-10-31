:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(library(http/http_cors)).
:- encoding(utf8).

% Iniciar o servidor na porta especificada
servidor(Port) :-
    http_server(http_dispatch, [port(Port)]).

:- consult("motorInferencia.pl").
:- consult("bc.pl").
:- consult("diagnostico.pl").
:- consult("escolha_carro.pl").

% Handlers para os diferentes endpoints.
% Predicados para Escolher Carro
:- http_handler(root(carros), http_handler_veiculos, [method(get)]).
:- http_handler(root(escolherCarro/marca), http_handler_listar_marcas, [method(get)]).
:- http_handler(root(escolherCarro/modelo), http_handler_listar_modelos, [method(post)]).
:- http_handler(root(escolherCarro/motor), http_handler_listar_motores, [method(post)]).
:- http_handler(root(obterNumeroCarro), http_handler_procurar_numero_carro, [methods([post, options])]).
:- http_handler(root(selecionarCarro), http_handler_escolher_carro, [methods([post, options])]).

% Diagnostico
:- http_handler(root(pergunta), http_handler_pergunta, [method(get)]).
:- http_handler(root(factos), http_handler_factos, [method(get)]).
:- http_handler(root(factosTodos), http_handler_factos_todos, [method(get)]).
:- http_handler(root(responder), http_handler_responder, [methods([post, options])]).
:- http_handler(root(diagnostico), http_handler_diagnostico, [method(get)]).

% Esclarecimentos
:- http_handler(root(como), http_handler_como, [methods([get, options])]).
:- http_handler(root(porque), http_handler_porque, [methods([post, options])]).
:- http_handler(root(porqueNao), http_handler_whynot, [methods([post, options])]).
:- http_handler(root(diagnosticoPossiveis), http_handler_diagnostico_possiveis, [method(get)]).


log_message(Message) :-
    open('server.log', append, Stream),
    get_time(TimeStamp),
    format_time(atom(FormattedTime), '%Y-%m-%d %H:%M:%S', TimeStamp),
    format(Stream, '~w: ~w~n', [FormattedTime, Message]),
    close(Stream).

% Configuração manual dos cabeçalhos CORS
cors_headers :-
    format('Access-Control-Allow-Origin: http://localhost:3000~n'),
    format('Access-Control-Allow-Methods: GET, POST, OPTIONS~n'),
    format('Access-Control-Allow-Headers: Content-Type~n').

% Predicados para Escolher Carro

% Handler para listar info dos Carros
http_handler_veiculos(Request) :-
    memberchk(method(options), Request),
    !,                                    
    cors_headers,
    format('~n').

% Handler para listar info dos Carros
http_handler_veiculos(_Request) :-
    cors_headers,
    veiculos_json(VeiculosJSON),
    reply_json(VeiculosJSON).
	
% Define a lista de todos os veículos em JSON.
veiculos_json(Veiculos) :-
    findall(Veiculo, veiculo_json(Veiculo), Veiculos).
	
% Cria o JSON de cada veículo com os componentes em formato de lista de pares.
veiculo_json(_{marca: _{nome: Marca}, modelo: _{nome: Modelo}, motor: _{nome: Motor}, componentes: Componentes}) :-
    carro(Id, Marca, Modelo, Motor),
    componentes_json(Id, Componentes).

% Obtém todos os componentes para o veículo específico.
componentes_json(Id, [
    _{nome: "motor_arranque", min: 0, minIdeal: MinA, maxIdeal: MaxA, max: Max1, unidade: "V"},
    _{nome: "bloco_motor", min: 0, minIdeal: MinB, maxIdeal: MaxB, max: Max2, unidade: "HP"},
    _{nome: "bateria", min: 0, minIdeal: MinC, maxIdeal: MaxC, max: Max3, unidade: "Ah"},
    _{nome: "liquido_arrefecimento", min: 0, minIdeal: MinD, maxIdeal: MaxD, max: Max4, unidade: "L"},
    _{nome: "oleo_motor", min: 0, minIdeal: MinE, maxIdeal: MaxE, max: Max5, unidade: "L"},
    _{nome: "fluido_travao", min: 0, minIdeal: MinF, maxIdeal: MaxF, max: Max6, unidade: "L"},
    _{nome: "fluido_transmissao", min: 0, minIdeal: MinG, maxIdeal: MaxG, max: Max7, unidade: "L"}
]) :-
    motor_arranque(Id, MinA, MaxA, Max1),
    bloco_motor(Id, MinB, MaxB, Max2),
    bateria(Id, MinC, MaxC, Max3),
    liquido_arrefecimento(Id, MinD, MaxD, Max4),
    oleo_motor(Id, MinE, MaxE, Max5),
    fluido_travao(Id, MinF, MaxF, Max6),
    fluido_transmissao(Id, MinG, MaxG, Max7).	
	
% Handler para listar marcas
http_handler_listar_marcas(_Request) :-
    listar_marcas(MarcasUnicas),
    reply_json(MarcasUnicas).

% Handler para listar modelos
http_handler_listar_modelos(Request) :-
    http_read_json_dict(Request, JsonIn),
    atom_string(Marca, JsonIn.marca),
    listar_modelos(Marca, ModelosUnicos),
    reply_json(ModelosUnicos).

% Handler para listar motores
http_handler_listar_motores(Request) :-
    http_read_json_dict(Request, JsonIn),
    atom_string(Marca, JsonIn.marca),
    atom_string(Modelo, JsonIn.modelo),
    listar_motores(Marca, Modelo, MotoresUnicos),
    reply_json(MotoresUnicos).

% Handler para obter id do carro
http_handler_procurar_numero_carro(Request) :-
    memberchk(method(options), Request),
    !,                                    
    cors_headers,
    format('~n').
	
http_handler_procurar_numero_carro(Request) :-
    cors_headers,  
    http_read_json_dict(Request, JsonIn),
    atom_string(Marca, JsonIn.marca),
    atom_string(Modelo, JsonIn.modelo),
    atom_string(Motor, JsonIn.motor),
    log_message('Dados obtidos: ' + Marca + Modelo + Motor),
    (   carro(Numero, Marca, Modelo, Motor)
    ->  reply_json(_{ numero: Numero })
    ;   reply_json(_{ error: 'Carro não encontrado' }, [status(404)])
    ).

% Handler para escolher o carro
http_handler_escolher_carro(Request) :-
    memberchk(method(options), Request),
    !,                                    
    cors_headers,
    format('~n').
	
http_handler_escolher_carro(Request) :-
    cors_headers,
    http_read_json_dict(Request, JsonIn),
    Numero = JsonIn.numero,
    obter_numero_carro(Numero, Carro),
	reply_json(_{ carro_escolhido: Carro }).

% Predicados para Diagnostico

% Handler para pergunta
http_handler_pergunta(Request) :-
    memberchk(method(options), Request),
    !,                                    
    cors_headers,
    format('~n').
	
http_handler_pergunta(_Request) :-
	cors_headers,
    obter_perguntas(Testes),
    ( Testes = [Teste|_] ->  % Verifica se há mais testes
        criacao_pergunta(Teste, Pergunta, OpcoesValidas),
        reply_json(json{pergunta: Pergunta, respostas: OpcoesValidas, estado: "ongoing"})
    ;   % Caso não haja mais perguntas
        reply_json(json{estado: "finalizado"})
    ).

% Converter cada facto para texto sem mutação
facto_to_text(Facto, Texto) :-
    term_to_atom(Facto, Texto).

% Handler para mostrar todos os factos filtrados
http_handler_factos(_Request) :-
    % Filtrar factos que não contenham proximo_teste, diagnostico ou solucao
    mostra_factos_filtrados(Factos),
	maplist(facto_to_text, Factos, FactosJson),
    reply_json(FactosJson).

% Handler para mostrar todos os factos
http_handler_factos_todos(_Request) :-
	mostra_factos(Factos),
	maplist(facto_to_text, Factos, FactosJson),
    reply_json(FactosJson).

http_handler_responder(Request) :-
    memberchk(method(options), Request),
    !,                                    
    cors_headers,
    format('~n').

% Handler para resposta
http_handler_responder(Request) :-
    cors_headers,
    obter_perguntas(Perguntas),
    length(Perguntas, N),
    (   N = 0
    ->  % Se a lista está vazia, retornar erro
        reply_json(_{estado: "finalizado", message: "Não há perguntas para responder"})
    ;   % Caso contrário, continuar
        http_read_json_dict(Request, JsonIn),
        (   (string(JsonIn.resposta) ; atom(JsonIn.resposta))
        ->  % Se for uma string ou átomo, converte para átomo se necessário
            atom_string(Resposta, JsonIn.resposta),
            reply_json(_{estado: "OK", message: "Respondido"}),
            % Chamar diagnostico_passo_passo/1 após responder
            catch(diagnostico_passo_passo(Resposta), Erro, log_message("Erro ao chamar diagnostico_passo_passo: ~w", [Erro]))
        ;   number(JsonIn.resposta)
        ->  % Se for um número, atribuir diretamente
            Resposta = JsonIn.resposta,
            reply_json(_{estado: "OK", message: "Respondido"}),
            % Chamar diagnostico_passo_passo/1 após responder
            catch(diagnostico_passo_passo(Resposta), Erro, log_message("Erro ao chamar diagnostico_passo_passo: ~w", [Erro]))
        ;   % Se não for nem string nem número, falhar com uma mensagem de erro
            reply_json(_{estado: "erro", message: "O campo 'resposta' deve ser string ou número"}),
            fail  % Usar fail para parar a execução neste caso
        )
    ).

% Handler para obter diagnostico possivel solucao
http_handler_diagnostico(Request) :-
    memberchk(method(options), Request),
    !,                                    
    cors_headers,
    format('~n').

http_handler_diagnostico(_Request) :-
	cors_headers,
    % Filtrar factos diagnostico e solucao
    mostra_diagnostico_solucao(Diagnostico, Solucao),
    Resposta = _{
        diagnostico: Diagnostico,
        solucao: Solucao
    },
    reply_json(Resposta).
	
% Handler para explicacao como
http_handler_como(Request) :-
    memberchk(method(options), Request),
    !,                                    
    cors_headers,
    format('~n').

http_handler_como(_Request) :-
    cors_headers, 
    como_response(1, Response),
    reply_json(Response).

% Handler para explicacao porque
http_handler_porque(Request) :-
    memberchk(method(options), Request),
    !,                                    
    cors_headers,
    format('~n').

http_handler_porque(Request) :-
	cors_headers,
    http_read_json_dict(Request, JsonIn),
    atom_string(FactoAtom, JsonIn.facto),      % Convertendo o fato JSON em átomo
    term_to_atom(FactoTerm, FactoAtom),        % Convertendo o átomo em termo Prolog
    porque_response(FactoTerm, Explicacao),
    reply_json(Explicacao).

% Handler para explicacao porque nao
http_handler_whynot(Request) :-
    memberchk(method(options), Request),
    !,                                    
    cors_headers,
    format('~n').

http_handler_whynot(Request) :-
    cors_headers,
    http_read_json_dict(Request, JsonIn),
    (   _{facto: FactoString} :< JsonIn
    ->  atom_string(FactoAtom, FactoString),
        term_to_atom(FactoTerm, FactoAtom)  % Conversão direta da string para termo
    ;   reply_json_dict(_{error: 'Erro ao criar o porque nao'}, [status(400)])
    ),
    with_output_to(string(Output), whynot(FactoTerm)),
    reply_json_dict(_{explanation: Output}).


% Handler para diagnosticos possiveis
http_handler_diagnostico_possiveis(Request) :-
    memberchk(method(options), Request),
    !,                                    
    cors_headers,
    format('~n').

http_handler_diagnostico_possiveis(_Request) :-
	cors_headers,
    listar_diagnosticos_possiveis(Diagnosticos),        % Chama o predicado para obter os diagnósticos
    prolog_to_json(Diagnosticos, DiagnosticosJSON),     % Converte para JSON
    reply_json(DiagnosticosJSON).                       % Envia a resposta JSON

% Converte a lista de diagnósticos para um formato JSON
prolog_to_json(Lista, json{diagnosticos: Lista}).
	
% Utilidade para começar o servidor na porta 8080 (pode ser ajustada)
:- servidor(4040).