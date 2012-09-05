-module(params).

-compile([{parse_transform,decorators}]).


-export([simple/2]).

%-decorate({?MODULE,mul_and_add,[10,3]}).
%-decorate(fun mul_and_add/2, [10,3]).

-decorate([10000]).

%% н
%-decorate([fun erlang:now/0, 2]).
simple(A, B) ->
    A+B.

% mul_and_add(F,Args,Arg1,Arg2) ->
%     R = F(Args),
%     R*Arg1+Arg2.
