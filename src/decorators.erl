-module(decorators).

-export([
    parse_transform/2,
    decorate/4
]).


parse_transform(Ast,Options)->
    gen_decorator:transform(Ast, [
        {module,   ?MODULE},
        {name,     decorate}
        |Options]
    ).

decorate(Function, Fargs, A, _info) ->
    10 * erlang:apply(Function, Fargs) + A.
    