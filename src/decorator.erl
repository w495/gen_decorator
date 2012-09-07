%%
%% @file    decorator.erl
%%          Реализация настоящего декоратора на основе gen_decorator.
%%          Этот декоратор делегирует декорирование другим 
%%          модулям (субдекораторам), которые передаются 
%%          как аргументы декоратора.
%%          
%%          ЗАЧЕМ:
%%          Например описан следующий модуль:
%%              
%%              01|-module(some_mod).
%%              02|-compile([{parse_transform, dec_mtenandadd}]).
%%              03|-compile([{parse_transform, dec_log}]).
%%              04|
%%              05|-log([]).
%%              06|-mtenandadd(3).
%%              07|-mtenandadd(2).
%%              08|-mtenandadd(1).
%%              09|-log([]).
%%              10|identity(A) when is_integer(A)->
%%              11|     A.
%%              
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
%%              
%%              01|-module(some_mod).
%%              02|-compile([{parse_transform, decorator}]).
%%              03|
%%              04|
%%              05|-dec(dec_log).
%%              06|-dec({dec_mtenandadd, [3]}).
%%              07|-dec({dec_mtenandadd, [2]}).
%%              08|-dec({dec_mtenandadd, [1]}).
%%              09|-dec(dec_log).
%%              10|identity(A) when is_integer(A)->
%%              11|     A.
%%              
%%          Переписывать модули декораторов dec_mtenandadd и dec_log
%%          не придется, разве что, дать им более короткие имена.
%%          При трансформации, аттрибут -dec(...) развертывается 
%%          в цепочку функций, последняя из которых вызывает
%%          decorate/4 текущего модуля. А она в свою очередь
%%          вызывает decorate/4 модулей, переданных в качестве аргументов 
%%          -dec(...). Для краткости, эти модули мы стали называть 
%%          субдекораторами. 
%%          Код после его причесывания, и переименования модулей субдекораторов
%%          имеет вид:
%%              
%%              01|-module(some_mod).
%%              02|-compile([{parse_transform, decorator}]).
%%              03|
%%              04|-dec(log).
%%              05|-dec({mtenandadd, 3}).
%%              06|-dec({mtenandadd, 2}).
%%              07|-dec({mtenandadd, 1}).
%%              08|-dec(log).
%%              09|identity(A) when is_integer(A)->
%%              10|     A.
%%              
-module(decorator).

%% Удовлетворяем требованиям gen_decorator,
%% a именно, определяем parse_transform/2, decorate/4.
%% Но сам атрибут behaviour не ставим, потому что rebar собирает этот модуль
%% до сборки gen_decorator, и выдает предупреждение.
%% %-behaviour(gen_decorator).

%% ==========================================================================
%% Экспортируемые функции
%% ==========================================================================

-export([
    %% Возвращает функциональный интерфейс.
    behaviour_info/1
]).

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

%% Возвращает функциональный интерфейс.
-spec   behaviour_info(_) ->
            undefined | [{decorate, 4}].

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
            Subdecorator    ::  subdecorator(),
            %% Информация о декорируемой функции.
            %% Отличается от erlang:fun_info(Function),
            %% тем что описывает не Function, а самую первую
            %% декорируемую фунцию.
            Function_info   ::  gen_decorator:function_info()
        ) ->    any().

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
    [{decorate, 4}];
behaviour_info(_) ->
    undefined.

%% @doc     Преобразует синтаксическое дерево. 
%%          Нужна для того, чтобы этот модуль можно было использовать 
%%          в качестве декоратора напрямую, с помощью атрибута модуля 
%%          -compile([{parse_transform, ...}]). 
%%          Интерфейсная функция gen_decorator. 
%%
%% @spec    parse_transform(erl_parse:abstract_form(), gen_decorator:decorator_options()) ->
%%              erl_parse:abstract_form().
%%
parse_transform(Ast, _options)->
    gen_decorator:transform(Ast, [
        {module,   ?MODULE},
        {name,     ?MODULE}
    ]).

%% @doc     Декорирует. Делегирует декорирование модулям переданным 
%%          как аргументы декоратора (субдекораторам).
%%          Это позволяет выполнять декорирование исходной функции
%%          в рамках одного преобразования parse_transform.
%%          Интерфейсная функция gen_decorator. 
%%
%% @spec    decorate(function(),[any()],subdecorator(),function_info()) ->
%%              any().
%%
%% @param   Function        function()          декорируемая функция.
%% @param   Function_args   [any()]             список аргументов функции.
%% @param   Subdecorator    subdecorator()      аргументы декоратора.
%% @param   Function_info   function_info()     информация об исходной функции.
%%
decorate(Function, Fargs, {Decorator, Darg}, Options)
    when erlang:is_atom(Decorator) ->
    erlang:apply(Decorator, decorate, [Function, Fargs, Darg, Options]);
decorate(Function, Fargs, Decorator, Options)
    when erlang:is_atom(Decorator)->
    erlang:apply(Decorator, decorate, [Function, Fargs, [], Options]).

