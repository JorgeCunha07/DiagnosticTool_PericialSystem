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
:- http_handler(root(responder), responder_handler, [method(post)]).
:- http_handler(root(porque), http_handler_porque, [method(post)]).
:- http_handler(root(factos), factos_handler, [method(get)]).
:- http_handler(root(pergunta), pergunta_handler, [method(get)]).
:- http_handler(root(como), como_handler, [method(get)]).
:- http_handler(root(diagnostico), diagnostico_handler, [method(get)]).

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
    Numero = JsonIn.numero,
    procurar_carro(Numero, Carro),
    log_message('Carro selecionado: ' + Carro),
    retractall(carro_selecionado(_)),
    assertz(carro_selecionado(Carro)),
    retractall(facto(_, _)),
    assertz(facto(1, proximo_teste(Numero, problemas))),
    reply_json(_{ carro_escolhido: Carro }).

http_handler_porque(Request) :-
    http_read_json_dict(Request, JsonIn),
    atom_string(FactoAtom, JsonIn.facto),      % Convertendo o fato JSON em átomo
    term_to_atom(FactoTerm, FactoAtom),        % Convertendo o átomo em termo Prolog
    porque_response(FactoTerm, Explicacao),
    reply_json(Explicacao).

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
                explicacao: "O facto é verdadeiro",
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

procurar_carro(Numero, Carro) :-
    carro(Numero, Marca, Modelo, Motor),
    format(atom(Carro), '~w ~w ~w', [Marca, Modelo, Motor]).

factos_handler(_Request) :-
    % Filtrar factos que não contenham proximo_teste, diagnostico ou solucao
    findall(Descricao, (facto(_, Descricao), \+ (Descricao = proximo_teste(_, _); Descricao = diagnostico(_, _); Descricao = solucao(_, _))), Factos),
    maplist(facto_to_text, Factos, FactosJson),
    reply_json(FactosJson).

% Converter cada facto para texto sem mutação
facto_to_text(Facto, Texto) :-
    term_to_atom(Facto, Texto).

pergunta_handler(_Request) :-
    findall(P, facto(_, proximo_teste(_, P)), [Teste|_]),
    functor(TesteTermo, Teste, 2),
    pergunta(TesteTermo, Pergunta),
    reply_json(Pergunta).


como_handler(_Request) :-
    como_response(1, Response),
    reply_json(Response).

como_response(N, Response) :-
    NSeguinte is N + 1,
    (   justifica(N, ID, _)
    ->  facto(N, F),
        F =.. [_, _, Regra],
        pergunta(F, PerguntaFormatada),
        % Transformar o termo composto em um átomo para JSON
        term_to_atom(F, FAtom),
        term_to_atom(Regra, RegraAtom),
        % Criação de uma estrutura de resposta em JSON para a pergunta atual
        Current = _{
            pergunta: PerguntaFormatada,
            resposta: RegraAtom,
            fato: FAtom,
            regra: ID
        },
        facto(NSeguinte, FSeguinte),
        FSeguinte =.. [PerguntaSeguinte, _, RespostaSeguinte],
        (   PerguntaSeguinte == diagnostico
        ->  term_to_atom(RespostaSeguinte, DiagnosticoAtom),
            Response = [Current, _{diagnostico: DiagnosticoAtom}]
        ;   como_response(NSeguinte, NextResponse),
            Response = [Current | NextResponse]
        )
    ;   Response = []
    ).


        
responder_handler(Request) :-
    % Verifica se há perguntas por responder
    findall(P, facto(_, proximo_teste(_, P)), Perguntas),
    length(Perguntas, N),
    
    (   N = 0
    ->  % Se a lista está vazia, retornar erro
        reply_json(_{status: "erro", message: "Não há perguntas para responder"})
    ;   % Caso contrário, continuar
        http_read_json_dict(Request, JsonIn),
        
        (   string(JsonIn.resposta)
        ->  % Se for uma string, converter diretamente para atom
            atom_string(Resposta, JsonIn.resposta),
            % Chamar diagnostico2/1 após responder
            catch(diagnostico2(Resposta), Erro,
                    log_message("Erro ao chamar diagnostico2: ~w", [Erro]))
        ;   number(JsonIn.resposta)
        ->  % Se for um número, atribuir diretamente
            Resposta = JsonIn.resposta,
            % Chamar diagnostico2/1 após responder
            catch(diagnostico2(Resposta), Erro,
                    log_message("Erro ao chamar diagnostico2: ~w", [Erro]))
        ;   % Se não for nem string nem número, falhar com uma mensagem de erro
            reply_json(_{status: "erro", message: "O campo 'resposta' deve ser string ou número"}),
            fail  % Usar fail para parar a execução neste caso
        )
    ).


diagnostico_handler(_Request) :-
    % Filtrar factos diagnostico e solucao
    findall(D, facto(_, diagnostico(_, D)), Diagnostico),
    findall(S, facto(_, solucao(_, S)), Solucao),
    Resposta = _{
        diagnostico: Diagnostico,
        solucao: Solucao
    },
    reply_json(Resposta).
	


% Utilidade para começar o servidor na porta 8080 (pode ser ajustada)
:- servidor(8080).
