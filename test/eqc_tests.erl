-module(eqc_tests).
-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include("riak_context.hrl").

%%-include_lib("eqc/include/eqc_statem.hrl").

-define(NUM_TESTS, 1000).

-define(RIAK_LOGGING_DEFAULT_CATEGORY,   "com.basho.client.c").
-define(RIAK_LOGGING_MAX_LEN,            256).
-define(RIAK_HOST_MAX_LEN,               256).


test_all() ->
    eqc_test_utils:generate_stubs(),
    riak_context_op(fun (CtxPP, _CtxP) ->
                      _Result = riak:riak_ping(CtxPP),
                      %%io:format("Result = ~p~n", [Result])
                      ok
                    end).


riak_context_op(Fn) ->
    {CtxPP, CtxP, HostnameP, PortP, Err} = make_context("localhost", "10017"),
    io:format("Result = ~p~n", [Err]),
    Result = Fn(CtxPP, CtxP),
    eqc_c:free(PortP),
    eqc_c:free(HostnameP),
    eqc_c:free(CtxP),
    eqc_c:free(CtxPP),
    %riak_context:riak_context_free(CtxPP),
    Result.


make_context(Hostname, Port) ->
    CtxPP   = eqc_c:alloc({ptr, "riak_context"}),
    CtxP     = eqc_c:alloc("riak_context"),
    eqc_c:store(CtxPP, CtxP),
    HostnameP = eqc_c:create_string(Hostname),
    PortP     = eqc_c:create_string(Port),
    Err = riak_context:riak_context_new_default(CtxPP, Hostname, Port),
    {CtxPP, CtxP, HostnameP, PortP, Err}.

term_string(S, Length) ->
    eqc_test_utils:padded_string(S, Length, 0).

create_context(Hostname, Port) ->
   #'_riak_context'{malloc_fn = {ptr,{func,{ptr,void}, [unsigned_long]}, 0},
                 realloc_fn = {ptr,{func,{ptr,void}, [{ptr,void},unsigned_long]}, 0},
                 free_fn = {ptr,{func,void,[{ptr,void}]},0},
                 pb_allocator = {ptr,{struct,'_ProtobufCAllocator'},0},
                 logging_category = lists:duplicate(256,0),
                 base = {ptr,{struct,event_base},0},
                 hostname = term_string(Hostname, ?RIAK_HOST_MAX_LEN),
                 portnum =  term_string(Port, ?RIAK_HOST_MAX_LEN),
                 addrinfo = {ptr,{struct,addrinfo},0}}.


