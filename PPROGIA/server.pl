:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- encoding(utf8).

% Iniciar o servidor na porta especificada
servidor(Port) :-
    http_server(http_dispatch, [port(Port)]).

:- consult("motorInferencia.pl").
:- consult("bc.pl").
:- consult("diagnostico.pl").
:- consult("escolha_carro.pl").

% Handlers para diferentes endpoints com método GET
:- http_handler(root(escolherCarro), http_handler_escolher_carro, [method(post)]).
:- http_handler(root(escolherCarro/marca), http_handler_listar_marcas, [method(get)]).
:- http_handler(root(escolherCarro/modelo), http_handler_listar_modelos, [method(post)]).
:- http_handler(root(escolherCarro/motor), http_handler_listar_motores, [method(post)]).
:- http_handler(root(obterNumeroCarro), http_hanlder_procurar_numero_carro, [method(post)]).
:- http_handler(root(diagnostico), diagnostico2_handler, []).

log_message(Message) :-
    open('server.log', append, Stream),
    get_time(TimeStamp),
    format_time(atom(FormattedTime), '%Y-%m-%d %H:%M:%S', TimeStamp),
    format(Stream, '~w: ~w~n', [FormattedTime, Message]),
    close(Stream).

% Handler para listar marcas
http_handler_listar_marcas(_Request) :-
    findall(Marca, carro(_, Marca, _, _), Marcas),
    list_to_set(Marcas, MarcasUnicas),
    reply_json(MarcasUnicas).

% Handler para listar modelos
http_handler_listar_modelos(Request) :-
    http_read_json_dict(Request, JsonIn),
    atom_string(Marca, JsonIn.marca),
    findall(Modelo, carro(_, Marca, Modelo, _), Modelos),
    list_to_set(Modelos, ModelosUnicos),
    reply_json(ModelosUnicos).

% Handler para listar motores
http_handler_listar_motores(Request) :-
    http_read_json_dict(Request, JsonIn),
    atom_string(Marca, JsonIn.marca),
    atom_string(Modelo, JsonIn.modelo),
    findall(Motor, carro(_, Marca, Modelo, Motor), Motores),
    list_to_set(Motores, MotoresUnicos),
    reply_json(MotoresUnicos).

% Handler para obter número do carro
http_hanlder_procurar_numero_carro(Request) :-
    http_read_json_dict(Request, JsonIn),
    atom_string(Marca, JsonIn.marca),
    atom_string(Modelo, JsonIn.modelo),
    atom_string(Motor, JsonIn.motor),
    log_message('Dados obtidos: ' + Marca + Modelo + Motor),
    (   carro(Numero, Marca, Modelo, Motor)
    ->  reply_json(_{ numero: Numero })
    ;   reply_json(_{ error: 'Carro não encontrado' }, [status(404)])
    ).

http_handler_escolher_carro(Request) :-
    http_read_json_dict(Request, JsonIn),
    atom_string(Numero, JsonIn.numero),
    log_message('Dados recebidos: ' + Numero),
    carro(Numero, Marca, Modelo, Motor),
    format(atom(Carro), '~w ~w ~w', [Marca, Modelo, Motor]),
    log_message('Carro selecionado: ' + Carro),
    retractall(carro_selecionado(_)),
    assertz(carro_selecionado(Carro)),
    retractall(facto(_, _)),
    assertz(facto(1, proximo_teste(Numero, problemas))).
        
diagnostico2_handler(_Request) :-
    % Chamar a função diagnostico2
    (   diagnostico2
    ->  % Se a função for bem-sucedida, retornar uma resposta JSON de sucesso
        reply_json_dict(_{status: "success", message: "Diagnóstico realizado com sucesso"})
    ;   % Caso contrário, retornar uma resposta JSON de erro
        reply_json_dict(_{status: "error", message: "Falha ao realizar diagnóstico"})
    ).

% Utilidade para começar o servidor na porta 8080 (pode ser ajustada)
:- servidor(8080).
