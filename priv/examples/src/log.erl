-module(log).
-behaviour(gen_subdecorator).

-export([
    decorate/4
]).

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
    fun()->
        Res
    end.
    
    