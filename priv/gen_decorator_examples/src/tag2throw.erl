-module(tag2throw).
-behaviour(gen_subdecorator).


-export([
    decorate/4,
    tag2throw/1
]).

decorate(Function, Fargs, Itype, Info) ->
    tag2throw(erlang:apply(Function, Fargs), Itype, Info).

tag2throw(Res)->
    tag2throw(Res, [], []).

tag2throw(Res, Itype, Info)->
    case Res of
        {ok, Result} ->
            %% Если результат есть, значит все ок.
            %% Тэг ok смысла не имеет.
            Result;
        {error, Error} ->
            throw(info(Itype, Error, Info))
    end.

info({Mod, Fun}, Error, Info) ->
    Mod:Fun(Error, Info);

info(info, Error, Info) ->
    {Error, Info};

%% default
info([], Error, _info) ->
    Error;
    
info(_, Error, Info)->
    info([], Error, Info).