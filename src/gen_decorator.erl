%%
%% @file    gen_decorator.erl   
%%          Реализация обощенного декоратора (generic decorator).
%%          Для создания декоратора необходимо в модуле декоратора
%%          определить функции parse_transform/2 и decorate/4.
%%          
%%          Например, декоратор журналирования можно описать так:
%%          <code>
%%              -module(dec_log).
%%              -behaviour(gen_decorator).
%%              -export([parse_transform/2, decorate/4]).
%%              
%%              parse_transform(Ast,Options)->
%%                  gen_decorator:transform(Ast,[{module,?MODULE},{name,log}]).
%% 
%%              decorate(Function, Fargs, [], Info) ->
%%                  Res = erlang:apply(Function, Fargs),
%%                  io:format("fun = ~p, res = ~p~n", [Info, Res]),
%%                  Res;
%%              decorate(Function, Fargs, Comment, Info) ->
%%                  Res = erlang:apply(Function, Fargs),
%%                  io:format(
%%                      "fun = ~p, res = ~p comment = ~p ~n", 
%%                      [Info, Res, Comment]
%%                  ),
%%                  Res.
%%          </code>
%%          Ниже пример использования такого декоратора.
%%          <code>
%%              -module(some_mod).
%%              -compile([{parse_transform, dec_log}]).
%%              -export([identity/1, tuplize/3]}.
%%
%%              -log([]).
%%              identity(A) -> A.
%%              
%%              -log(some_comment).
%%              tuplize(A, B, C) -> {A, B, C}.
%%          </code>
%% 
-module(gen_decorator).

%% ==========================================================================
%% Экспортируемые функции и типы
%% ==========================================================================

-export([
    %% Преобразует синтаксическое дерево. Интерфейсная функция. Нужна для того,
    %% чтобы этот модуль можно было использовать в качестве декоратора.
    %% Должна быть переопределена в настоящих декораторах.
    parse_transform/2,
    %% Преобразует синтаксическое дерево. Эту функцию следует использовать 
    %% для определения parse_transform/2 в настоящих декораторах.
    transform/2,
    %% Занимается непосредственно декорированием.
    %% Функция является частью функционального интерфейса этого модуля.
    %% Должна быть определена в настоящих декораторах.
    %% Оставлена, для отладки и тестирования декораторов и в качестве примера.
    %% В текущей реализации просто вычисляет декорируемую функцию.
    decorate/4
]).

-export([
    %% Возвращает функциональный интерфейс.
    behaviour_info/1 
]).

-export_type([
    %% Насройки декоратора.
    decorator_options/0,
    %% Информация о декорируемой функции.
    function_info/0
]).

%% ==========================================================================
%% Спецификации 
%% ==========================================================================

%% @doc Насройки декоратора:
%%  * name     --- имя декоратора, как атрибута модуля;
%%  * module   --- имя модуля, в котором опрелен декоратор;
%%  * function --- декорирующая функция.
%% Если при создании декоратора, какая-то из настроек не указана, 
%% то берется настройка по-умолчанию.
%% По-умолчанию:
%%  * name      = decorate;
%%  * module    = ?MODULE, этот модуль;
%%  * function  = decorate.
%%
-type decorator_options() ::
    [{name, atom()}|{module, atom()}|{function, atom()}].

%% @doc Информация о декорируемой функции.
%% В отличие от erlang:fun_info/1 призвана описывать именно исходную функцию,
%% а не ту, которая была передана в декоратор.
-type function_info()  ::
    [{arity,integer()}|{name,atom()}|{module,atom()}|{line,integer()}].

%% Преобразует синтаксическое дерево.
%% Интерфейсная функция
%% Псевдоним для transform/2.
-spec   parse_transform(
            Ast         ::erl_parse:abstract_form(), %% Синтаксическое дерево.
            Options     ::list(compile:options())    %% Насройки трансформации
        ) ->    erl_parse:abstract_form().           %% Новое дерево.

%% Преобразует синтаксическое дерево.
-spec   transform(
            Ast         ::erl_parse:abstract_form(), %% Синтаксическое дерево.
            Options     ::decorator_options()        %% Насройки декоратора
        ) ->    erl_parse:abstract_form().           %% Новое дерево

%% Занимается непосредственно декорированием.
-spec   decorate(
            %% Декорируемая функция.
            Function        ::  function(),
            %% Список аргументов декорируемой функции.
            Function_args   ::  [any()],
            %% Aргументы декоратора. Может быть только один
            %% (т.к. мы в рамках синтаксических правил erlang)
            %% Потому мы pекомендуем передавать аргументы в списке.
            Decorator_args  ::  any(),
            %% Информация о декорируемой функции.
            %% Отличается от erlang:fun_info(Function),
            %% тем что описывает не Function, а самую первую 
            %% декорируемую фунцию.
            Function_info   ::  function_info()
        ) ->    any().

%% Возвращает функциональный интерфейс.
-spec   behaviour_info(_) ->
            undefined | [{parse_transform, 2}|{decorate, 4}, ... ].

%% ==========================================================================
%% Внутрениие структуры
%% ==========================================================================

%% ----------------------------------------------------------------
%% Некоторые абстрактные формы, для удобной работы
%% ----------------------------------------------------------------

%% @doc     Абстрактная форма конца файла.
%%
%% @private
%%
-record(eof, {
    line    :: integer()
}).

%% @doc     Абстрактная форма внешней функции.
%%
%% @private
%%
-record(remote, {
    line     :: integer(),
    module   :: atom(),
    function :: atom()
}).

%% @doc     Абстрактная форма вызова функции.
%%
%% @private
%%
-record(call, {
    line        :: integer(),
    function    :: erl_parse:abstract_form(),
    args        :: [erl_parse:abstract_form()]
}).

%% @doc     Абстрактная форма уравнения.
%%
%% @private
%%
-record(clause, {
    line    :: integer(),
    args    :: [{var,Line::integer(),Arg::atom()}],
    guards  :: [],
    body    :: [erl_parse:abstract_form()]
}).

%% @doc     Абстрактная форма функции.
%%
%% @private
%%
-record(function, {
    line    :: integer(),
    name    :: atom(),
    arity   :: integer(),
    clauses :: list(record(clause))
}).

%% @doc     Абстрактная форма атрибута модуля.
%%
%% @private
%%
-record(attribute, {
    line  :: integer(),
    name  :: atom(),
    value :: any()
}).

%% ----------------------------------------------------------------
%% Внутренние структуры модуля
%% ----------------------------------------------------------------

%% @doc Состояние преобразования. 
%%      В структуре хранятся настройки декораторов 
%%      и некоторые промежуточные результаты.
%%
%% @private
%%
-record(dstate, {
    module   = ?MODULE   :: atom(), %% Модуль настоящего декоратора.
    name     = decorate  :: atom(), %% Имя настоящего декоратора.
    function = decorate  :: atom(), %% Декорирующая функция.
    fmod     = undefined :: atom(), %% Модуль декорируемой функции.
    dlist    = []        :: list(record(attribute)), %% Список декораторов.
    dargs    = []        :: list(any()),  %% Аргументы декоратора.
    argnms   = []        :: list(atom()), %% Имена аргументов функций.
    argabs   = []        :: list(atom()), %% Абстрактные представления функции.
    dllen    = 0         :: integer(),    %% Длина cписока декораторов.
    dind     = 0         :: integer()     %% Номер текущего декоратора.
}).

%% ==========================================================================
%% Константы и макросы
%% ==========================================================================

-ifdef(DEBUG).
    -ifndef(debug).
        -define(debug(T, F),
            io:format(
                "~ndebug(~p):~n~80.80c~n" ++ T ++ "~n~80.80c~n",
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
        -define(debug(W,T,F),[]).
        -define(debug(T,F),[]).
    -endif. % debug
    -ifndef(pprint).
        -define(pprint(Ast),[]).
    -endif. % pprint
-endif. % DEBUG

%% ==========================================================================
%% Внешние функции
%% ==========================================================================

%% @doc     Возвращает описания поведения.
%%          Описание функционального интерфейса модуля декораторов.
%%
%% @spec    behaviour_info(_) ->
%%              undefined | [{parse_transform, 2}|{decorate, 4}].
%%
behaviour_info(callbacks) ->
    [{parse_transform, 2},{decorate, 4}];
behaviour_info(_) ->
    undefined.

%% @doc     Преобразует синтаксическое дерево.
%%          Интерфейсная функция. Нужна для того, чтобы этот модуль 
%%          можно было использовать в качестве декоратора напрямую, 
%%          с помощью атрибута модуля -compile([{parse_transform, ...}]). 
%%          Должна быть переопределена в настоящих декораторах.
%%
%% @spec    parse_transform(
%%              erl_parse:abstract_form(), 
%%              list(compile:options())
%%          ) -> erl_parse:abstract_form().
%%
parse_transform(Ast,_options)->
    gen_dec:transform(Ast, [
        {module,   ?MODULE},
        {name,     decorate}
    ]).

%% @doc     Декорирует. Функция является частью функционального интерфейса 
%%          этого модуля. Должна быть определена в настоящих декораторах.
%%          Оставлена, для отладки, тестирования декораторов и как пример
%%          определения подобных функций в настоящих декораторах.
%%          В текущей реализации просто вычисляет декорируемую функцию.
%%
%% @spec    decorate(function(),[any()],any(),function_info()) ->
%%              any().
%%
%% @param   Function        function()          декорируемая функция.
%% @param   Function_args   [any()]             список аргументов функции.
%% @param   Decorator_args  any()               аргументы декоратора.
%% @param   Function_info   function_info()     информация об исходной функции.
%%
decorate(Function, Fargs, _dargs, _options) ->
    ?debug("Function",          "~p", [Function]),
    ?debug("Function  args",    "~p", [Fargs]),
    ?debug("Decorator args",    "~p", [_dargs]),
    ?debug("Decorator options", "~p", [_options]),
    erlang:apply(Function, Fargs).


%% @doc     Преобразует синтаксическое дерево. Эту функцию следует использовать 
%%          для определения parse_transform/2 в настоящих декораторах.
%%
%% @spec    transform(erl_parse:abstract_form(), decorator_options()) ->
%%              erl_parse:abstract_form().
%% 
transform(Ast, Options)->
    ?debug("OLD AST",       "~p",   [Ast]),
    ?debug("OLD FORMS",     "~s",   [?pprint(Ast)]),
    Dstate = #dstate{
        name        =   proplists:get_value(name,       Options,    decorate),
        module      =   proplists:get_value(module,     Options,    ?MODULE),
        function    =   proplists:get_value(function,   Options,    decorate)
    },
    {East, Rdecs} = lists:mapfoldl(fun ntr/2,Dstate,Ast),
    Nast = lists:flatten([
        lists:filter(fun(Node)-> Node =/= nil end, East),
        %% добавляем в синтаксическое дерево,
        %% сообщения о пустых декораторах.
        rogue(Rdecs)
    ]),
    ?debug("NEW AST",       "~p",   [Nast]),
    ?debug("NEW FORMS",     "~s",   [?pprint(Nast)]),
    Nast.

%% ==========================================================================
%% Внутрениие функции
%% ==========================================================================

%% @doc     Возвращает список ошибок о наличии пустых декораторов.
%%          Это нужно для отлова пустых декораторов. 
%%          Например, в конце файла.
%%
%% @spec    rogue(record(dstate))->
%%              [{error, {integer(), atom(), [atom(), string()]}}]
%%
rogue(#dstate{dlist=Dlist})->
    [   {error,{Line,erl_parse,[rogue_dec, io_lib:format("~p",[Value])]}}
        || #attribute{line=Line, value=Value} <- Dlist
    ].


%% @doc     Преобразует узлы уровня модуля возвращает пустой узел (nil),
%%          единственный узел, или список узлом. Пустые узлы удаляются 
%%          при следующем проходе и списки спрямляются (flatten).
%%
%% @spec    ntr(erl_parse:abstract_form(), record(dstate))->
%%              {erl_parse:abstract_form(), record(dstate)}
%%
ntr(#attribute{name=module, value=Module}=Node, #dstate{}=Dstate) ->
    %% Выяснем имя модуля для функции, которую декорируем.
    %% Здесь можно было бы собрать больше информации о модуле,
    %% но пока это кажется не очень нужным
    {Node, Dstate#dstate{fmod=Module}};

ntr(#attribute{name=Name}=Node,#dstate{name=Name,dlist=Dlist}=Dstate) ->
    %% Аккумулируем все декораторы одной функции.
    {nil, Dstate#dstate{dlist=[Node|Dlist]}};

ntr(#function{}=Node, #dstate{dlist=[]}=Dstate) ->
    %% Пропускаем функцию без декораторов.
    {Node, Dstate#dstate{dlist=[]}};

ntr(#function{}=Node, Dstate) ->
    %% Применяем декораторы.
    {applydecs(Node, Dstate), Dstate#dstate{dlist=[]}};

ntr(#eof{}=Node, Dstate) ->
    %% Обрабатываем ошибки.
    {[Node| rogue(Dstate) ], Dstate#dstate{dlist=[]}};

ntr(Node, Dstate) ->
    %% Все остальное.
    {Node, Dstate}.

%% @doc     Применяет декораторы.
%%          Добавляет в синтаксическое дерево модуля новые функции,
%%          Заменяет декорируемую функцию на вызов цепочки этих новых функций.
%%          Концом цепочки является тело старой функции.
%%
%% @spec    applydecs(record(function), record(dstate))->
%%              [erl_parse:abstract_form()]
%%
applydecs(#function{arity=Arity,line=Line}=Node,#dstate{dlist=Dlist}=Odstate)
    when erlang:length(Dlist) > 0 ->
    %% Все что можем вычислить сейчас, вычисляем,
    %% чтобы не делать одно и то же несколько раз.
    Argnames = [to_atom(['Arg', Anum]) || Anum <- lists:seq(1,Arity)],
    Dstate = Odstate#dstate{
        dllen   = erlang:length(Dlist),
        argnms  = Argnames,
        argabs  = args(Line, Argnames)
    },
    %%
    %% В этом списке оказался важен порядок,
    %% При применении нескольких разных декораторов, от разных parse_transform.
    %%
    [
        %% Функия с именем оригинальной функии;
        %% для вызова цепочки декораторов.
        trampoline(Node, Dstate),
        %% Оригинальная функция, но с новым именем.
        original(Node, Dstate)
        %% Цепочка декораторов, 
        %% функций каскадно вызывающих декорирующие функии.
        | dchain(Node,Dstate)
    ].

%% @doc     Возвращает замену оригинальной функции, 
%%          переадресовывая вызов на цепь декораторов.
%%          Количество аркументов не меняется.
%%          Важно отметить, что от оригинальной функции 
%%          в новую одноименную функцию переходит только имя. 
%%          Все сопоставления с образцом, и оградительные выражения 
%%          остаются с переименованной старой функцией.
%%
%% @spec    trampoline(record(function), record(dstate))-> record(function)
%% 
trampoline(#function{line=Line}=Node,#dstate{dllen=Len,argabs=Argabs}=Dstate) ->
    Node#function{
        clauses = [
            #clause{
                line    =   Line,
                args    =   Argabs,
                guards  =   [],
                body    =   [
                    #call{ %% Вызов последнего декоратора в цепи.
                        line        =   Line,
                        function    =   {atom, Line, wfname(Node, Dstate,Len)},
                        args        =   Argabs
                    }
                ]
            }
        ]
    }.

%% @doc     Переименовывает оригинальную функцию.
%%          Эта переименованная функция будет перво в цепочке декораторов.
%%
%% @spec    original(record(function), record(dstate))-> record(function)
%%
original(#function{}=Node, Dstate) ->
    Node#function{name=rfname(Node, Dstate)}.

%% @doc     Возвращает цепочку декораторов для добавления 
%%          в синтаксическое дерево.
%%
%% @spec    dchain(record(function), record(dstate))-> [record(function)]
%%
dchain(Node, #dstate{dlist=Dlist, dllen=Len}=Dstate) ->
    Dinds = lists:zip(Dlist, lists:seq(1, Len)),
    [
        dec(Node,Dstate#dstate{dargs=Dargs,dind=Dind})
        || { #attribute{value=Dargs},Dind} <- Dinds
    ].

%% @doc     Возвращает один декоратор.
%%          Количество аркументов декоратора и исходной функции совпадает.
%%
%% @spec    dec(record(function), record(dstate))-> record(function)
%%
dec(#function{line=Line}=Node,#dstate{dind=Dind,argabs=Argabs}=Dstate) ->
    Node#function{
        name    =   wfname(Node, Dstate, Dind),
        clauses =   [
            #clause{
                line   = Line,
                args   = Argabs,
                guards = [],
                body   = [dbody(Node, Dstate)]
            }
        ]
    }.

%% @doc     Возвращает тело декоратора. А именно, вызов декорирующей функции. 
%%          Важно отметить, что сам по себе декоратор, как в python,
%%          Возвращает функцию, а не значение. Эта функция передается дальше
%%          другим декораторам. Здесь, нам это делать не обязательно,
%%          так как согласно построению, мы в любом случаю, передадим такую
%%          функию (ответсвенность за это берет сам декоратор, которуму
%%          мы должны эту функцию передать). Потому здесь,
%%          при вызове декоратора возвращается значение декорирующей функции.
%%          Помимо всего прочего, это позволяет, сэкономить один вызов.
%%
%% @spec    dec(record(function), record(dstate))-> record(сall)
%%
dbody(  #function{line=Line, name=Ofname, arity=Arity}=Node,
        #dstate{
            function = Function,
            module   = Module,
            dargs    = Dargs,
            fmod     = Fmod,
            dind     = Dind,
            argnms   = Argnames
        }=Dstate
    ) ->
    #call{
        line=Line,
        function= #remote{
            line     = Line,
            module   = {atom,Line,Module},
            function = {atom,Line,Function}
        },
        args=[
            %% Декорируемая функция
            {'fun',Line,{function, wfname(Node, Dstate, Dind-1), Arity}},
            %% Аргументы Функции
            arglist(Line, Argnames),
            %% Аргументы декоратора
            vals(Line, Dargs),
            %% Дополнительная информация
            %% о декорируемой функции.
            vals(Line, [
                %% значение арности, нам уже изветсно,
                %% повторно вычислять не придется,
                %% если понадобится
                {arity,     Arity},
                {name,      Ofname},
                {module,    Fmod},
                {line,      Line}
            ])
        ]
    }.


%% @doc     Возвращет абстрактное представление аргументов функции 
%%          на основе их списка. Сами аргументы задаются атомов вида 'Arg'
%%
%% @spec    args(integer(), list(atom()))-> 
%%              list({var,integer(),atom()}::erl_parse:abstract_form).
%%
args(Line, List) ->
    [{var,Line,Arg} || Arg <- List].

%% @doc     Возвращет абстрактное представление списка аргументов функции
%%          на основе их списка. Сами аргументы задаются атомов вида 'Arg'
%%          В отличие от предыдущей функции конструируется правильный список.
%%
%% @spec    args(integer(), list(atom()))->
%%              erl_parse:abstract_form.
%%
arglist(Line, List) ->
    lists:foldr(
        fun(Arg, Acc) ->
            {cons, Line, {var, Line, Arg}, Acc}
        end,
        {nil,Line},
        List
    ).

%% @doc     Возвращет абстрактное представление списка значений.
%%          Нужна для подстановки в явном виде аргументов декоратора.
%%
%% @spec    vals(integer(), list(atom()))->
%%              erl_parse:abstract_form.
%%
vals(Line,List) ->
    erl_parse:abstract(List, Line).

%% @doc     Возвращет новое имя для переданной исходной функции.
%%          Эта функция будет перво в цепочке декораторов.
%%
%% @spec    rfname(record(function), record(dstate))-> atom().
%%
rfname(#function{name=Ofname, arity=Arity}, #dstate{name=Dname}) ->
    to_atom(['<', Ofname, '/', Arity, '-dec-', Dname, '-0>']).

%% @doc     Возвращет имя для функции из цепочки декораторов.
%%          Number --- номер декоратора в цепочке.
%%
%% @spec    wfname(record(function), record(dstate), integer())-> atom().
%%
wfname(#function{name=Ofname, arity=Arity}, #dstate{name=Dname}, Number) ->
    to_atom(['<', Ofname, '/', Arity, '-dec-', Dname, '-', Number, '>']).

%% @doc     Собирает атом, из переданного списка термов.
%%          Не самая эффективная реализация, 
%%          но быстрее чем предыдущие варианты.
%%
%% @spec    to_atom(list(any()))-> atom()
%%
to_atom(Elements) ->
    erlang:binary_to_atom(
        erlang:list_to_binary(
            lists:map(
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
            )
        ),
        utf8
    ).

% При текущей последовательности функций в цепи декораторов 
% (определена в applydecs/2), надобность в этой функции отпала.
% Но она может пригодится во время эксплуатации модуля.
% 
% %% @doc     Из имени декорированной функции выделяет имя исходной функции. 
% %%          Если функция была декорирована несколько раз, разными декораторами,
% %%          то ее имя в верхних (последних) декораторах теряется.
% %%          Это не удивительно, потому что разныые parse_transform не 
% %%          знают друг о друге. pfname/1 решает эту проблему.
% %%
% %% @spec    pfname(atom(), record(dstate))-> atom().
% %%
% pfname(Ofname, _Dstate)->
%     erlang:list_to_atom(
%         re:replace(
%             erlang:atom_to_list(Ofname),
%             "(<)+||(/\\d-dec-.+-\\d>)+", [],
%             [{return, list}, global]
%         )
%     ).
% 
%
