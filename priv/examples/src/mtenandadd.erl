-module(mtenandadd).
-behaviour(decorator).

-export([
    decorate/4
]).


decorate(Function, Fargs, [A], _info) ->
    10 * erlang:apply(Function, Fargs) + A.
    