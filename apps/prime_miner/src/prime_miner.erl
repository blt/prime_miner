-module(prime_miner).

-behaviour(gen_fsm).

%% API
-export([start_link/0]).

%% gen_fsm callbacks
-export([init/1, loop/2, loop/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-ifdef(TEST).
-export([next_prime/1]).
-endif.

-record(state, {primes :: [non_neg_integer()], timeout :: non_neg_integer()}).

%% ===================================================================
%%  API
%% ===================================================================

start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ===================================================================
%%  gen_fsm callbacks
%% ===================================================================

init([]) ->
    error_logger:info_msg("Starting the mining loop!~n"),
    {ok, loop, #state{primes=lists:reverse([2,3,5,7,11,13]), timeout=10000}, 0}.

loop(timeout, #state{primes=Primes, timeout=Timeout}=S) ->
    Prime = next_prime(Primes),
    {next_state, loop, S#state{primes=[Prime|Primes]}, Timeout}.

loop({request, largest_prime}, From, #state{primes=[P|_], timeout=Timeout}=S) ->
    gen_fsm:reply(From, P),
    {next_state, loop, S, Timeout}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, #state{}=S, [from1to2]) ->
    {ok, StateName, S#state{timeout=100}};
code_change(_OldVsn, StateName, #state{}=S, [from2to1]) ->
    {ok, StateName, S#state{timeout=10000}}.

%% ===================================================================
%%  Internal functions
%% ===================================================================

-spec next_prime(Primes::[non_neg_integer()]) -> non_neg_integer().
next_prime([P|_Rest]=Primes) ->
    next_prime(Primes, P+1).

-spec next_prime(Primes::[non_neg_integer()], Candiate::non_neg_integer()) ->
                        non_neg_integer().
next_prime(Primes, Candiate) ->
    case lists:any(fun(P) -> Candiate rem P =:= 0 end, Primes) of
        false -> Candiate;
        true -> next_prime(Primes, Candiate+1)
    end.
