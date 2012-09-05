-module(decorators).

-export([
    parse_transform/2,
    decorate/4
]).


parse_transform(Ast,Options)->
    gen_dec:transform(Ast, [
        {module,   ?MODULE},
        {name,     decorate}
        |Options]
    ).

decorate(Function, Fargs, [Arg1], _options) ->
    fun()->
        R = erlang:apply(Function, Fargs),
        R*Arg1
    end;

decorate(Function, Fargs, [Arg1,   Arg2], _options) ->
    fun()->
        R = erlang:apply(Function, Fargs),
        R*Arg1+Arg2
    end.
