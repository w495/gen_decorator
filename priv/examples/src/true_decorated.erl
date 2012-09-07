-module(true_decorated).

% -compile([{parse_transform, dec_log}]).
% -compile([{parse_transform, dec_mtenandadd}]).

-compile([{parse_transform, decorator}]).

-export([
    identity/1,
    test_identity/0
]).

%-decorate({?MODULE,mul_and_add,[10,3]}).
%-decorate(fun mul_and_add/2, [10,3]).

-dec(dec_log).
-dec({dec_mtenandadd, [3]}).
-dec({dec_mtenandadd, [2]}).
-dec({dec_mtenandadd, [1]}).
-dec(dec_log).
identity(A) when is_integer(A)->
    A.

%% н
%-decorate([fun erlang:now/0, 2]).
% 
% -log([]).
% -mtenandadd([3]).
% -mtenandadd([2]).
% -mtenandadd([1]).
% -log([]).
% identity(A) when is_integer(A)->
%     A.
% 
% 
test_identity() ->
    9000123 == params:identity(9000).

