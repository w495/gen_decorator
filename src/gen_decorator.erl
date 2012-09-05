-module(gen_decorator).

%% ==========================================================================
%% Экспортируемые функции
%% ==========================================================================

-export([
    behaviour_info/1
]).

-export([
    %% Преобразует синтаксическое дерево. Интерфейсная функция.
    %% Нужна для того, чтобы этот модуль можно было использовать
    %% в качестве декоратора. 
    %% Должна быть переопределена в настоящих декораторах
    parse_transform/2,
    %% Преобразует синтаксическое дерево.
    transform/2,
    decorate/4
]).

%% ==========================================================================
%% Экспортируемые функции
%% ==========================================================================

%% @doc Настройки декораторв
%% @private
-record(dopts, {
    module      =   ?MODULE     :: atom(),
    name        =   decorate    :: atom(),
    function    =   decorate    :: atom()
}).


%% @doc Настройки декораторв
%% @private
-record(dfun, {
    old_fname   :: atom(),
    new_fname   :: atom(),
    arity       :: integer()
}).


-define(DEBUG, true).

-ifdef(DEBUG).
    -ifndef(debug).
        -define(debug(T, F),
            io:format(
                "~ndebug(~p):~n~80.80c~n" ++ F ++ "~n~80.80c~n",
                [?LINE,$=|F]++[$-]
            )
        ).
        -define(debug(W,T,F),
            io:format(
                "~ndebug(~p):~s~n~80.80c~n" ++ T ++ "~n~80.80c~n",
                [?LINE,W,$=|F]++[$-]
            )
        ).
    -endif. % debug
    -ifndef(pprint).
        -define(pprint(Ast),erlang:list_to_binary([erl_pp:form(N) || N<-Ast])).
    -endif. % pprint
-else.
    -ifndef(debug).
        -define(debug(F,A),[]).
    -endif. % debug
    -ifndef(pprint).
        -define(pprint(A),[]).
    -endif. % pprint
-endif. % DEBUG



-spec   behaviour_info(_) ->
            undefined | [{parse_transform, 2} | {decorate, 4}, ... ].

-spec   parse_transform(
            Ast         ::erl_parse:abstract_form(),
            Options     ::proplists:proplist()
        ) ->
            erl_parse:abstract_form().

-spec   transform(
            Ast         ::erl_parse:abstract_form(),
            Options     ::proplists:proplist()
        ) ->
            erl_parse:abstract_form().

-spec   decorate(
            Function        ::  function(),
            Function_args   ::  [any()],
            Decorator_args  ::  [any()],
            Options         ::  proplists:proplist()
        ) -> fun().


%% @doc Возвращает описания OTP-поведения.
%% 
%% Описание функционального интерфейса модуля декораторв
%%
behaviour_info(callbacks) ->
    [
        {parse_transform, 2},
        {decorate, 4}
    ];

behaviour_info(_) ->
    undefined.


parse_transform(Ast,Options)->
    gen_dec:transform(Ast, [
        {module,   ?MODULE},
        {name,     decorate}
        |Options]
    ).

decorate(Function, Fargs, _dargs, _options) ->
    fun()->
        ?debug("Function",          "~p", [Function]),
        ?debug("Function  args",    "~p", [Fargs]),
        ?debug("Decorator args",    "~p", [_dargs]),
        ?debug("Decorator options", "~p", [_options]),
        erlang:apply(Function, Fargs)
    end.



% TODO: сообщения об ошибках в декораторе
% parse_transform(Ast,_options)->
%     parse_transform(Ast, fun transform_node/2, _options).

transform(Ast, Options)->
    ?debug("OLD AST",       "~p",   [Ast]),
    ?debug("OLD FORMS",    "~s",   [?pprint(Ast)]),
    Dopts = #dopts{
        name        =   proplists:get_value(name,       Options,    decorate),
        module      =   proplists:get_value(module,     Options,    ?MODULE),
        function    =   proplists:get_value(function,   Options,    decorate)
    },
    
    {East, Rdecs} = lists:mapfoldl(
        fun(Item, Acc) ->
            transform_node(Item, Acc, Dopts)
        end,
        [],
        Ast
    ),
    Nast =
        lists:flatten(
            lists:filter(fun(Node)-> Node =/= nil end, East)
        )
    ++ emit_errors_for_rogue_decorators(Rdecs, Dopts),
    ?debug("NEW AST",       "~p",   [Nast]),
    ?debug("NEW FORMS",     "~s",   [?pprint(Nast)]),
    Nast.


emit_errors_for_rogue_decorators(Dlist, _dopts)->
    [   {error,
            {Line,
                erl_parse,
                [   "rogue decorator ",
                    io_lib:format("~p",[D])
                ]
            }
        }
        || {attribute, Line, _, D} <- Dlist
    ].

%
% преобразует узлы уровня модуля
% см http://www.erlang.org/doc/apps/erts/absform.html
% возвращает пустой узел (nil),
% единственный узел, или список узлом.
% пустые узлы удаляются при следующем проходе и списки спрямляются (flatten).
%
transform_node(Node={attribute, _Line, Name, _Decorator}, Dlist, #dopts{name=Name}) ->
    % Аккумулируем все декораторы одной функции
    {nil, [Node|Dlist]};
transform_node(Node={function, _Line, _Fname, _Arity, _Clauses}, [], _dopts) ->
    % Пропускаем функцию без декораторов
    {Node, []};
transform_node(Node={function, _Line, _Fname, _Arity, _Clauses}, Dlist, Dopts) ->
    % Декорируем
    {apply_decorators(Node,Dlist, Dopts), []};
    
transform_node(Node={eof,_Line}, Dlist, Dopts) ->
    {[Node| emit_errors_for_rogue_decorators(Dlist, Dopts) ], []};
    
transform_node(Node, Dlist, _dopts) ->
    % Все остальное
    {Node, Dlist}.

%%
%% @doc применяет декораторы
%%
apply_decorators(Node={function, Line, Fname, Arity, _Clauses}, Dlist, Dopts) when erlang:length(Dlist) > 0 ->
    Dfun = #dfun{old_fname=Fname, arity=Arity},
    [
        % Оригинальная, переименованная функция
        function_form_original(Node, Dopts),
        % Замена оригинальной функции на нашу цепочку декораторов
        function_form_trampoline(Line, Dfun, Dlist, Dopts),
        % Функция funname_arityn_0 
        % для преобразования входных параметров 
        % в единый список
        function_form_unpacker(Line,Dfun,Dopts)
        % Цепочка декораторов
        | function_forms_decorator_chain(Line, Dfun, Dlist,Dopts)
    ].


function_form_original({function, Line, Fname, Arity, Clauses}, _dopts) ->
    {function, Line, generated_func_name({original,Fname}), Arity, Clauses}.


% возвращает замену оригинальной функции, 
% переадресовывая вызов на цепь декораторов,
% заменяя входные аргументы на их список
function_form_trampoline(Line, #dfun{old_fname=Fname, arity=Arity}, Dlist, _dopts) ->
    Dnb = erlang:length(Dlist),
    Arg_names = arg_names(Arity),
    { function, Line, Fname, Arity,
        [{  clause,
            Line,
            emit_arguments(Line, Arg_names),
            emit_guards(Line, []),
            [
                emit_local_call( Line,
                    generated_func_name(
                        {decorator_wrapper, Fname, Arity, Dnb}
                    ),
                    emit_arguments(Line,Arg_names)
                )
            ]
        }]
    }.

% Функция обратная предыдущей,
% на вход получает список аргументов
% и вызывает оригинальную функцию
function_form_unpacker(Line,#dfun{old_fname=Fname, arity=Arity}, _dopts) ->
    Arg_names = arg_names(Arity),
    Ofun = generated_func_name({original,Fname}),
    {   function,
        Line,
        generated_func_name({decorator_wrapper, Fname, Arity, 0}),
        Arity,
        [{  clause,
            Line,
            %[emit_atom_list(Line, Arg_names)],
            emit_arguments(Line, arg_names(Arity)),
            emit_guards(Line, []),
            [{  call,
                Line,
                {atom,Line,Ofun},
                emit_arguments(Line,Arg_names)
            }]
        }]
    }.

function_forms_decorator_chain(Line, Dfun, Dlist, Dopts) ->
    Dnb = erlang:length(Dlist),
    Dindexes = lists:zip(Dlist, lists:seq(1, Dnb)),
    [
        function_form_decorator_chain(Line,Dfun,Dmf,Dindex, Dopts)
        || { {attribute,_,_,Dmf},Dindex} <- Dindexes
    ].

function_form_decorator_chain(
        Line,
        #dfun{old_fname=Fname, arity=Arity} = Dfun,
        Dmf,
        Dindex,
        Dopts
    ) ->
    Nfname = generated_func_name({decorator_wrapper, Fname, Arity, Dindex-1}),
    Ffun = 'F',
    {   function,
        Line,
        generated_func_name({decorator_wrapper, Fname,Arity, Dindex}), % name
        Arity,
        [{  clause,
            Line,
            emit_arguments(Line, arg_names(Arity) ),
            emit_guards(Line, []),
            [
                emit_decorated_fun(
                    Line,
                    Ffun,
                    Dfun#dfun{new_fname=Nfname},
                    Dmf,
                    Dopts
                ),
                {call, Line,{var,Line,Ffun},[]}
            ]
        }]
    }.

emit_decorated_fun_worker(
        Line,
        Name,
        #dfun{
            arity=Arity,
            old_fname=Ofname,
            new_fname=Nfname
        },
        Module,
        Function,
        Eargs,
        _dopts
    )
    when erlang:is_atom(Module), erlang:is_atom(Function)->
    {match,Line,
        {var,Line,Name},
        {call,
            Line,
            {   remote,
                Line,
                {atom,Line,Module},
                {atom,Line,Function}
            },
            [
                {'fun',Line,{function, Nfname, Arity}},
                emit_atom_list(Line, arg_names(Arity)),
                emit_values(Line, Eargs),
                emit_values(Line, [{fname, Ofname},{line, Line}])
            ]
        }
    }.

%%
%%
%%
emit_decorated_fun(
        Line,
        Name,
        Dfun,
        {Module, Function},
        Dopts
    ) when erlang:is_atom(Module), erlang:is_atom(Function) ->
    emit_decorated_fun_worker(
        Line,
        Name,
        Dfun,
        Module,
        Function,
        [],
        Dopts
    );

%%
%%
%%
emit_decorated_fun(
        Line,
        Name,
        Dfun,
        {Module,Function,Eargs},
        Dopts
    ) when erlang:is_atom(Module), erlang:is_atom(Function) ->
    emit_decorated_fun_worker(
        Line,
        Name,
        Dfun,
        Module,
        Function,
        Eargs,
        Dopts
    );

emit_decorated_fun(
        Line,
        Name,
        Dfun,
        Eargs,
        #dopts{
            function    =   Function,
            module      =   Module
        } = Dopts
    )  ->
    emit_decorated_fun_worker(
        Line,
        Name,
        Dfun,
        Module,
        Function,
        Eargs,
        Dopts
    ).

emit_local_call(Line, Fname, List) ->
    {call, Line, {atom, Line, Fname}, List}.

emit_arguments(Line, List) ->
    [{var,Line,Arg} || Arg <- List].

emit_values(Line,List) ->
    erl_parse:abstract(List, Line).


emit_guards(_Line, [])->
    [];

emit_guards(_,_)->
    throw(nyi).

emit_atom_list(Line, List) ->
    lists:foldr(
        fun(Arg, Acc) ->
            {cons, Line, {var, Line, Arg}, Acc}
        end,
        {nil,Line},
        List
    ).

generated_func_name( {original, Oname} ) ->
    atom_name(["<", Oname, ".o>"]);
generated_func_name( {trampoline, Oname} ) ->
    Oname;
generated_func_name( {decorator_wrapper, Oname, Arity, N} ) ->
    atom_name(["<", Oname, ".a{", Arity, "},v{", N, "}>"]).

% list() -> atom()
atom_name(Elements) ->
    list_to_atom(lists:flatten(lists:map(
        fun
            (A) when is_atom(A) ->
                erlang:atom_to_list(A);
            (A) when is_integer(A) ->
                erlang:integer_to_list(A);
            (A) when is_binary(A) ->
                erlang:binary_to_list(A);
            (A) when is_list(A) ->
                io_lib:format("~s",[A])
        end,
        Elements
    ))).
 
arg_names(Arity) ->
    [ atom_name(["Arg", Anum]) || Anum <- lists:seq(1,Arity)].
