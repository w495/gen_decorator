-module(throw2tag).
-behaviour(gen_subdecorator).

-export([
    decorate/4,
    throw2tag/1
]).

decorate(Function, Fargs, Itype, Info) ->
    throw2tag(erlang:apply(Function, Fargs), Itype, Info).

throw2tag(Res)->
    throw2tag(Res, [], []).

throw2tag(Res, Itype, Info)->
    try
        {ok, Res}
    catch
        Class:Excepton ->
            info(Itype, {Class, Excepton}, Info)
    end.

info({Mod, Fun}, {Class, Exception}, Info) ->
    Mod:Fun({Class, Exception}, Info);

info(info, {Class, Exception}, Info) ->
    {Class, {Exception, Info}};

info(class, {Class, Exception}, _info) ->
    {Class, Exception};

%% default
info([], {_class, Exception}, _info) ->
    {error, Exception};
    
info(_, {Class, Exception}, Info) ->
    info([], {Class, Exception}, Info).
