-module(sub_decorated).


-compile([{parse_transform, decorator}]).

-export([
    identity/1,
    identitydl/1
]).

-decorator(log).
-decorator({mtenandadd, [3]}).
-decorator({mtenandadd, [2]}).
-decorator({mtenandadd, [2]}).
-decorator(log).
identity(A) when is_integer(A)->
    A.

-decorator([
    log,
    {mtenandadd, [3]},
    {mtenandadd, [2]},
    {mtenandadd, [2]},
    log
]).
identitydl(A) when is_integer(A)->
    A.


test_identity() ->
    9000123 == ?MODULE:identity(9000).

test_identitydl() ->
    9000123 == ?MODULE:identitydl(9000).
