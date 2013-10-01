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
    Deps = ["libevent",
            "protobuf",
            "libprotobuf-c"],
    DashLLibs = ["riak_c_client",
             "event_core",
             "event_extra",
             "iconv",
             "log4c",
             "pthread",
             "protobuf",
             "protobuf-c"],
    DashL0 = lists:map(fun (L) -> "-l" ++ L end, DashLLibs),
    DashL =  string:join(DashL0, " "),
    ProjectL = ["-L../build"],
    ProjectI = ["-I.",
                "-I../src/include",
                "-I../build/include",
                "-I../build/proto"],
    Libs0 = lists:map(fun (L) ->
                        os:cmd("pkg-config --libs-only-L " ++ L)
                      end,
                      Deps),
    Libs = re:replace(string:join(Libs0, " "),"\n", " ", [global, {return, list}])
           ++ string:join(ProjectL, " "),
    Incs0 = lists:map(fun (L) ->
                        os:cmd("pkg-config --cflags-only-I " ++ L)
                      end,
                      Deps),
    Incs = re:replace(string:join(Incs0, " "), "\n", " ", [global, {return, list}])
           ++ string:join(ProjectI," "),
    Objs = filelib:wildcard("../build/riak_*.o") ++
           filelib:wildcard("../build/call_backs.o") ,
    Params = [ {c_src,"../src/riak.c"},
               definitions_only,
               {additional_files, Objs},
               {cflags, DashL ++ " " ++ Libs},
               {cppflags, Incs}],
    io:format(user,"~p", [Params]),
    eqc_c:start(riak, Params).
    %%eqc:quickcheck(eqc:numtests(NumTests, prop_main())).

-endif.
-endif.




