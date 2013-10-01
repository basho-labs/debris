-module(eqc_tests).

-compile(export_all).

-ifdef(EQC).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


-define(NUM_TESTS, 1000).

eqc_test_() ->
    [?_assert(eqc_helper() =:= true)].
prop_main() ->
    ?FORALL(L, list(int()),
            L == lists:reverse(lists:reverse(L))).

eqc_helper() ->
    eqc_helper(?NUM_TESTS).

eqc_helper(NumTests) ->
    eqc:quickcheck(eqc:numtests(NumTests, prop_main())).

-endif.
-endif.
