-module(params).

-compile([{parse_transform,decorators}]).


-export([simple/1]).

%-decorate({?MODULE,mul_and_add,[10,3]}).
%-decorate(fun mul_and_add/2, [10,3]).



%% н
%-decorate([fun erlang:now/0, 2]).

-decorate(3).
-decorate(2).
-decorate(1).
simple(A) when is_integer(A)->
    A.

% mul_and_add(F,Args,Arg1,Arg2) ->
%     R = F(Args),
%     R*Arg1+Arg2.
