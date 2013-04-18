-module(pm).

-export([largest_prime/0]).

-spec largest_prime() -> non_neg_integer().
largest_prime() ->
    gen_fsm:sync_send_event(prime_miner, {request, largest_prime}).
