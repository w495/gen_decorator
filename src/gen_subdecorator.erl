%%
%% @file    gen_subdecorator.erl
%%          
-module(gen_subdecorator).


%% ==========================================================================
%% Экспортируемые функции
%% ==========================================================================

-export([
    %% Возвращает функциональный интерфейс.
    behaviour_info/1
]).


%% ==========================================================================
%% Спецификации
%% ==========================================================================


%% Возвращает функциональный интерфейс.
-spec   behaviour_info(_) ->
            undefined | [{decorate, 4}].


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
