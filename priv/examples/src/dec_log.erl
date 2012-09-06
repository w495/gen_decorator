-module(dec_log).
-behaviour(gen_decorator).

-export([
    parse_transform/2,
    decorate/4
]).


parse_transform(Ast,Options)->
    io:format("Options = ~p", [Options]),
    gen_decorator:transform(Ast, [
        {module,   ?MODULE},
        {name,     log}
        |Options]
    ).

decorate(Function, Fargs, [], Info) ->
    Res = erlang:apply(Function, Fargs),
    spawn_link(fun()->
        error_logger:info_report(
            lists:flatten(
                io_lib:format(
                    "Line = ~p.~nFunc = ~p:~p/~p.~nArgs = ~p.~nRes = ~p.~n", [
                    proplists:get_value(line,   Info),
                    proplists:get_value(module, Info),
                    proplists:get_value(name,   Info),
                    proplists:get_value(arity,  Info),
                    Fargs,
                    Res
                ])
            )
        )
    end),
    Res.
    