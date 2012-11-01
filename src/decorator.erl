%%
%% @file    decorator.erl
%%          Реализация настоящего декоратора на основе gen_decorator.
%%          Этот декоратор делегирует декорирование другим  модулям 
%%          (субдекораторам), которые передаются как аргументы декоратора. 
%%          (Для создания субдекоратора достаточно определить функцию
%%          decorate/4 в теле модуля субдекоратора.
%%          
%%      Зачем:
%%          Например, описан следующий модуль:
%%          <code>
%%              -module(some_mod).
%%              -compile([{parse_transform, dec_mtenandadd}]).
%%              -compile([{parse_transform, dec_log}]).
%%              -export([identity/1]}.
%%
%%              -log([]).
%%              -mtenandadd(3).
%%              -mtenandadd(2).
%%              -mtenandadd(1).
%%              -log([]).
%%              identity(A) -> A.
%%          </code>    
%%          При выполнении функции some_mod:identity/1 сначала применятся 
%%          все сразу дектораторы mtenandadd, и только потом все сразу
%%          дектораторы log. В итоге, это не совсем то, что ожидается
%%          от данного кода. Проблема связана с тем, что настоящие
%%          декораторы применяются к исходной фукции в рамках разных 
%%          parse_transform. Порядок следования атрибутов модуля 
%%          -compile([{parse_transform, ...}]) и определяет 
%%          порядок применения декораторов.
%%          В некоторых случаях это может быть полезно. Но не во всех. 
%%          Описанный выше код, такая неявная особенность только
%%          запутывает. Чтобы решить проблему, нужно применять декораторы, 
%%          в одной parse_transform. Для этого и был создан текущей модуль.
%%          При использовании его, получаем:
%%          <code>  
%%              -module(some_mod).
%%              -compile([{parse_transform, decorator}]).
%%              -export([identity/1]}.
%%
%%              -decorator(dec_log).
%%              -decorator({dec_mtenandadd, [3]}).
%%              -decorator({dec_mtenandadd, [2]}).
%%              -decorator({dec_mtenandadd, [1]}).
%%              -decorator(dec_log).
%%              identity(A) -> A.
%%          </code>
%%          Переписывать модули декораторов dec_mtenandadd и dec_log
%%          не придется, разве что, дать им более короткие имена.
%%          При трансформации, аттрибут -decorator(...) развертывается
%%          в цепочку функций, последняя из которых вызывает
%%          decorate/4 текущего модуля. А она в свою очередь
%%          вызывает decorate/4 модулей, переданных в качестве аргументов 
%%          -decorator(...). Для краткости, эти модули мы стали называть
%%          субдекораторами. 
%%          Код после его причесывания, и переименования модулей субдекораторов
%%          имеет вид:
%%          <code>  
%%              -module(some_mod).
%%              -compile([{parse_transform, decorator}]).
%%              -export([identity/1]}.
%%
%%              -decorator(log).
%%              -decorator({mtenandadd, 3}).
%%              -decorator({mtenandadd, 2}).
%%              -decorator({mtenandadd, 1}).
%%              -decorator(log).
%%              identity(A) -> A.
%%          </code>
%%          или, более лаконично
%%          <code>
%%              -module(some_mod).
%%              -compile([{parse_transform, decorator}]).
%%              -export([identity/1]}.
%%
%%              -decorator([
%%                  log,
%%                  {mtenandadd, 3},
%%                  {mtenandadd, 2),
%%                  {mtenandadd, 1},
%%                  log
%%              ]).
%%              identity(A) -> A.
%%          </code>
%%
-module(decorator).

%% Удовлетворяем требованиям gen_decorator,
%% a именно, определяем parse_transform/2, decorate/4.
%% Обратите внимание, что при переименовании модуля,
%% порядок сборки у rebar может измениться. 
%% gen_decorator должен быть собран ДО текущего модуля.
%-behaviour(gen_decorator).


%% ==========================================================================
%% Экспортируемые функции
%% ==========================================================================

-export([
    %% Преобразует синтаксическое дерево. 
    %% Интерфейсная функция gen_decorator.
    parse_transform/2,
    %% Занимается непосредственно декорированием. 
    %% Интерфейсная функция gen_decorator. 
    %% Вызывает функцию decorate/4 субдекоратора.
    decorate/4
]).

-export_type([
    %% Описание субдекоратора.
    subdecorator/0
]).

%% ==========================================================================
%% Спецификации
%% ==========================================================================

%% Описание субдекоратора.
-type subdecorator() :: 
    Module::atom()|{Module::atom(), Arg::any()}.

%% Преобразует синтаксическое дерево.
%% Интерфейсная функция
%% Псевдоним для transform/2.
-spec   parse_transform(
            Ast         ::erl_parse:abstract_form(), %% Синтаксическое дерево.
            Options     ::list(compile:options())    %% Насройки трансформации
        ) ->    erl_parse:abstract_form().           %% Новое дерево.

%% Занимается непосредственно декорированием.
-spec   decorate(
            %% Декорируемая функция.
            Function        ::  function(),
            %% Список аргументов декорируемой функции.
            Function_args   ::  [any()],
            %% 
            %% 
            %% 
            Subdecorator    ::  subdecorator()|[subdecorator()],
            %% Информация о декорируемой функции.
            %% Отличается от erlang:fun_info(Function),
            %% тем что описывает не Function, а самую первую
            %% декорируемую фунцию.
            Function_info   ::  gen_decorator:function_info()
        ) ->    any().

%% ==========================================================================
%% Внешние функции
%% ==========================================================================

%% @doc     Преобразует синтаксическое дерево. 
%%          Нужна для того, чтобы этот модуль можно было использовать 
%%          в качестве декоратора напрямую, с помощью атрибута модуля 
%%          -compile([{parse_transform, ...}]). 
%%          Интерфейсная функция gen_decorator. 
%%
%% @spec    parse_transform(
%%              erl_parse:abstract_form(), 
%%              list(compile:options())
%%          ) -> erl_parse:abstract_form().
%%

% -decorator({
%     cache, 200, 12
% }).
% -decorator({log}).
% -decorator(cache).
% -decorator([{log,   []}]).

parse_transform(Ast, _options)->
    gen_decorator:transform(Ast, [
        {module,   ?MODULE},
        {name,     decorator}
    ]).

%% @doc     Декорирует. Делегирует декорирование модулям переданным 
%%          как аргументы декоратора (субдекораторам).
%%          Это позволяет выполнять декорирование исходной функции
%%          в рамках одного преобразования parse_transform.
%%          Интерфейсная функция gen_decorator. 
%%
%% @spec    decorate(
%%              function(),
%%              [any()],
%%              subdecorator()|[subdecorator()],
%%              function_info()
%%          ) ->
%%              any().
%%
%% @param   Function        function()          
%%              декорируемая функция.
%% @param   Function_args   [any()]             
%%              список аргументов функции.
%% @param   Subdecorator    subdecorator()|[subdecorator()]
%%              субдекоратор (модуль или модуль с аргументом), 
%%              которому делегируют декорирование исходной функции,
%%              таких модулей может быть несколько
%% @param   Function_info   function_info()     
%%              информация об исходной функции.
%%
decorate(Function, Fargs, {Subdecorator, Darg}, Options)
    when erlang:is_atom(Subdecorator) ->
    erlang:apply(Subdecorator, decorate, [Function, Fargs, Darg, Options]);

decorate(Function, Fargs, {Subdecorator}, Options)
    when erlang:is_atom(Subdecorator)->
    decorate(Function, Fargs, Subdecorator, Options);

decorate(Function, Fargs, Subdecorator, Options)
    when erlang:is_atom(Subdecorator)->
    erlang:apply(Subdecorator, decorate, [Function, Fargs, [], Options]);

decorate(Function, Fargs, Subdecorators, Options)
    when erlang:is_list(Subdecorators)->
    {decorated, Decorated} = lists:foldr(
        %%
        %% Основная проблема, в том, что,
        %% Арность функции, которую возвращает декоратор (fun/0),
        %% не совпадает арностью оригинальной функции (fun/1).
        %% Потому нужно над оригинальной функцие арганизовывать функциюобертку
        %% с произвольнной арностью.
        %% 
        %% Возможно, это будет заменено в следующих версиях, тем,
        %% что декораторы будут обязаны возвращать функцию той же арности,
        %% что и исходная. Так сделанно в python.
        %% На данный момент для erlang это представляется не очень удобным.
        %%
        %% Такой подход, с оборачиванием функции,
        %% может привести к некоторой потери производительности,
        %% т.к. код конструируется во время выполнения.
        %% Для ускорения, данный модуль, можно переписать,
        %% как чистый parse_transform, без делегирования модулю gen_decorator.
        %% 
        fun (Subdecorator, {original, Fun})->
                %% Для оригинальной фунции применяем все без изменения.
                {decorated, decorate(Fun, Fargs, Subdecorator, Options)};
            (Subdecorator, {decorated, Fun}) ->
                %% Для декорированной фунции, конструируем обертку
                %% и передаем ее вместо самой функции.
                {value, Dfun, _} =
                    erl_eval:expr(
                        {   'fun',?LINE,
                            {clauses,[
                                {clause,?LINE,
                                    lists:map(
                                        fun(_)->
                                            {var,?LINE,'_'}
                                        end,
                                        Fargs
                                    ),
                                    [],
                                    [{call,?LINE,{var,?LINE,'F'},[]}]
                                }
                            ]}
                        },
                        [{'F', Fun}]
                    ),
                {decorated, decorate(Dfun, Fargs, Subdecorator, Options)}
        end,
        {original, Function},
        Subdecorators
    ),
    Decorated.
