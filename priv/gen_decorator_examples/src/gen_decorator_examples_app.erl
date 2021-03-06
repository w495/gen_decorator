-module(gen_decorator_examples_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(gen_decorator_examples).
    
start(_StartType, _StartArgs) ->
    gen_decorator_examples_sup:start_link().

stop(_State) ->
    ok.
