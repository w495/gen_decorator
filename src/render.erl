-module(render).

-export([
    parse_transform/2,
    decorate/4
]).


parse_transform(Ast,Options)->
    gen_dec:transform(Ast, [
        {module,   ?MODULE},
        {name,     render},
        {function, render}
        |Options]
    ).

decorate(F, Args,   Arg1,   Arg2) ->
    R = F(Args),
    R*Arg1+Arg2.
