-module(mtenandadd).
-behaviour(gen_subdecorator).

-export([
    decorate/4
]).


decorate(Function, Fargs, [A], _info) ->
    fun()->
        10 * erlang:apply(Function, Fargs) + A
    end.
    