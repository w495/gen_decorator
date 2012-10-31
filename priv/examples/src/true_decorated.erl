-module(true_decorated).

-compile([{parse_transform, dec_log}]).
-compile([{parse_transform, dec_mtenandadd}]).

-export([
    identity/1,
    test_identity/0
]).


-log([]).
-mtenandadd([3]).
-mtenandadd([2]).
-mtenandadd([1]).
-log([]).
identity(A) when is_integer(A)->
    A.

test_identity() ->
    9000123 == ?MODULE:identity(9000).

