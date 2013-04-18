-module(prime_miner_tests).
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%%  next_prime/1
%% ===================================================================

next_prime_test_() ->
    [
     { "it computes 5 as a prime",
       ?_assertMatch(5, prime_miner:next_prime([3,2]))},
     { "it computes 7 as a prime as well",
       ?_assertMatch(7, prime_miner:next_prime([5,3,2]))}
    ].
