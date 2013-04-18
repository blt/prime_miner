-module(primes_app).

-behaviour(application).

%% Application callbacks
-export([start/1, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start([StartType, StartArgs]) ->
    start(StartType, StartArgs).

start(_StartType, _StartArgs) ->
    _ = application:start(prime_miner),
    _ = application:start(frontman),
    primes_sup:start_link().

stop(_State) ->
    ok.
