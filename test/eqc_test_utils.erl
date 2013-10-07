-module(eqc_test_utils).
-compile(export_all).

-include_lib("eqc/include/eqc.hrl").



test_all() ->
    eqc:quickcheck(prop_fixed_array()).

%% ---------------------------------------------------------------------------
%% @doc Generate a fixed size array of size Size, fill it with ListData,
%%     then fill the rest of the array with Filler
%%     This fn makes it convenient to populate a fixed size C character array
%%     from an Erlang string
%% ---------------------------------------------------------------------------
fixed_length_array_from_list(Size, ListData, Filler) ->
    FillerLen = Size - length(ListData),
    array:from_list(ListData ++ lists:duplicate(FillerLen, Filler)).


prop_fixed_array() ->
    ?FORALL({L, FillerLen},
            {list(int), ?SUCHTHAT(N, int(), N > 0)},
            begin
              NewA = fixed_length_array_from_list(length(L) + FillerLen, L, 0),
              array:size(NewA) == (length(L) + FillerLen)
            end).

%% ---------------------------------------------------------------------------

join_with_prefix(List, Prefix) ->
    re:replace(string:join(lists:map(fun (I) ->
                Prefix ++ I
                end, List), " "), "\n", " ",
                [global, {return, list}]).

join_with_fn_prefix(List, Fn) ->
    re:replace(string:join(lists:map(Fn, List), " "), "\n", " ",
                [global, {return, list}]).


exclude_from_obj_list(List, Atom) ->
  lists:filter(fun (F) ->
                  case re:run(F, erlang:atom_to_list(Atom) ++ "\.o") of
                    nomatch -> true;
                    _ -> false
                  end 
               end, List).


generate_stubs() ->
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

    ProjectL = join_with_prefix(["../build"],"-L"),
    ProjectI = join_with_prefix(
                    [".",
                    "../src/include",
                    "../build/include",
                    "../build/proto"],"-I"),
    Libs =  ProjectL ++ " " ++
                join_with_fn_prefix(Deps,
                          fun (L) ->
                            os:cmd("pkg-config --libs-only-L " ++ L) end),
   Incs = ProjectI ++ " " ++ 
                join_with_fn_prefix(Deps,
                          fun (L) -> 
                            os:cmd("pkg-config --cflags-only-I " ++ L) end),
    Objs = filelib:wildcard("../build/*.o"),

    Mods = [riak_error, riak_object, riak_pb_message, riak_utils, riak_log, riak_network, riak_event, riak, riak_binary, riak_context],
    lists:foreach(fun (M) ->
                    TheseObjs = exclude_from_obj_list( exclude_from_obj_list(Objs, M), main),
                    Params = [ {c_src,"../src/" ++ erlang:atom_to_list(M) ++ ".c"},
                        definitions_only,
                        {additional_files, TheseObjs},
                        {cflags, DashL ++ " " ++ Libs},
                        {cppflags, Incs}],
                    %io:format(user,"~p~n", [Params]),
                    io:format(user, "Gen eqc_c bindings for ~p~n",[M]),
                    eqc_c:start(M, Params)
                    end, Mods).



