-module(decgen).
-behaviour(gen_decorator).

-export([
    parse_transform/2,
    decorate/4
]).


parse_transform(Ast,Options)->
    gen_decorator:transform(Ast, [
        {module,   ?MODULE},
        {name,     dec}
        |Options]
    ).


decorate(Function, Fargs, {Decorator, Darg}, Options)
    when erlang:is_atom(Decorator) ->
    erlang:apply(Decorator, decorate, [Function, Fargs, Darg, Options]);


decorate(Function, Fargs, Decorator, Options)
    when erlang:is_atom(Decorator)->
    erlang:apply(Decorator, decorate, [Function, Fargs, [], Options]).