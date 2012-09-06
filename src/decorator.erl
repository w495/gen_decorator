%%
%% @file    decorator.erl
%%          Реализация настоящего декоратора на основе gen_decorator.
%%
%% 
-module(decorator).

%% Удовлетворяем требованиям gen_decorator,
%% a именно, определяем parse_transform/2, decorate/4.
%-behaviour(gen_decorator).

%% ==========================================================================
%% Экспортируемые функции
%% ==========================================================================

-export([
    %% Возвращает функциональный интерфейс.
    behaviour_info/1
]).

-export([
    %% Преобразует синтаксическое дерево. 
    %% Интерфейсная функция. 
    parse_transform/2,
    %% %% Занимается непосредственно декорированием. 
    %% %% Интерфейсная функция.
    decorate/4
]).


%% ==========================================================================
%% Спецификации
%% ==========================================================================

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
            %% Aргументы декоратора. Может быть только один
            %% (т.к. мы в рамках синтаксических правил erlang)
            %% Потому мы pекомендуем передавать аргументы в списке.
            Decorator_args  ::  any(),
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
%%          Интерфейсная функция. Нужна для того, чтобы этот модуль
%%          можно было использовать в качестве декоратора.
%%          Должна быть переопределена в настоящих декораторах.
%%
%% @spec    parse_transform(erl_parse:abstract_form(), gen_decorator:decorator_options()) ->
%%              erl_parse:abstract_form().
%%
parse_transform(Ast, _options)->
    gen_decorator:transform(Ast, [
        {module,   ?MODULE},
        {name,     ?MODULE}
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
decorate(Function, Fargs, {Decorator, Darg}, Options)
    when erlang:is_atom(Decorator) ->
    erlang:apply(Decorator, decorate, [Function, Fargs, Darg, Options]);
decorate(Function, Fargs, Decorator, Options)
    when erlang:is_atom(Decorator)->
    erlang:apply(Decorator, decorate, [Function, Fargs, [], Options]).

