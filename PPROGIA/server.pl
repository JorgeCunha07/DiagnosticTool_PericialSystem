:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- encoding(utf8).

% Iniciar o servidor na porta especificada
servidor(Port) :-
    http_server(http_dispatch, [port(Port)]).

:- consult("motorInferencia.pl").
:- consult("bc.txt").
:- consult("diagnostico.pl").
% :- consult("escolha_carro.pl").

% Handlers para diferentes endpoints
:- http_handler(root(carro), escolha_carro_handler, []).
:- http_handler(root(diagnostico), diagnostico2_handler, []).

log_message(Message) :-
    open('server.log', append, Stream),
    format(Stream, '~w~n', [Message]),
    close(Stream).

escolha_carro_handler(Request) :-
    catch(
        (   http_read_json_dict(Request, DictIn),
            process_request(DictIn, Response, History),
            reply_json_dict(Response)
        ),
        Error,
        (   log_message('Erro ao processar o pedido: ' + Error), 
            reply_json_dict(_{status: "error", message: "Ocorreu um erro no servidor"})
        )
    ).

% Função para processar a requisição do utilizador.
process_request(DictIn, Response, History) :-
    (   _{answer: Answer} :< DictIn
    ->  (   Answer == ""
        ->  ask_next_question(History, marca, Response)
        ;   NewHistory = [Answer|History],
            process_next_step(NewHistory, Response)
        )
    ;   log_message('JSON inválido: ' + DictIn),
        Response = _{status: "error", message: "JSON inválido"}
    ).

% Função para determinar a próxima etapa com base no histórico.
process_next_step(History, Response) :-
    length(History, N),
    (   N = 0
    ->  ask_next_question(History, marca, Response)
    ;   N = 1
    ->  ask_next_question(History, modelo, Response)
    ;   N = 2
    ->  ask_next_question(History, motor, Response)
    ;   N = 3
    ->  finalize_car_selection(History, Response)
    ).

% Função para perguntar a próxima questão ao utilizador.
ask_next_question(History, Step, Response) :-
    (   Step = marca
    ->  findall(Marca, carro(_, Marca, _, _), Marcas),
        sort(Marcas, MarcasUnicas),
        Question = "Escolha uma marca",
        Options = MarcasUnicas
    ;   Step = modelo,
        History = [Marca|_],
        atom_string(MarcaAtom, Marca),
        findall(Modelo, carro(_, MarcaAtom, Modelo, _), Modelos),
        sort(Modelos, ModelosUnicos),
        Question = "Escolha um modelo",
        Options = ModelosUnicos
    ;   Step = motor,
        History = [Marca, Modelo|_],
        atom_string(MarcaAtom, Marca),
        atom_string(ModeloAtom, Modelo),
        findall(Motor, carro(_, MarcaAtom, ModeloAtom, Motor), Motores),
        sort(Motores, MotoresUnicos),
        log_message('Motores encontrados para o modelo ' + ModeloAtom + ': ' + MotoresUnicos),
        Question = "Escolha um motor",
        Options = MotoresUnicos
    ),
    Response = _{
        question: Question,
        options: Options
    }.


% Função para finalizar a seleção do carro.
finalize_car_selection(History, Response) :-
    History = [Marca, Modelo, Motor],
    carro(Numero, Marca, Modelo, Motor),
    Response = _{
        status: "complete",
        message: "Seleção concluída",
        carro: _{
            numero: Numero,
            marca: Marca,
            modelo: Modelo,
            motor: Motor
        }
    }.

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
