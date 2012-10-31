-module(dec_mtenandadd).
-behaviour(gen_decorator).

-export([
    parse_transform/2,
    decorate/4
]).


parse_transform(Ast,Options)->
    gen_decorator:transform(Ast, [
        {module,   ?MODULE},
        {name,     mtenandadd}
        |Options]
    ).

decorate(Function, Fargs, [A], _info) ->
    fun()->
        10 * erlang:apply(Function, Fargs) + A
    end.