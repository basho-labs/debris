-module(eqc_tests).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include("riak_context.hrl").

%%-include_lib("eqc/include/eqc_statem.hrl").

-define(NUM_TESTS, 1000).

create_context() ->
   #'_riak_context'{malloc_fn = {ptr,{func,{ptr,void},
                                        [unsigned_long]}, 0},
                 realloc_fn = {ptr,{func,{ptr,void},
                                         [{ptr,void},unsigned_long]}, 0},
                 free_fn = {ptr,{func,void,[{ptr,void}]},0},
                 pb_allocator = {ptr,{struct,'_ProtobufCAllocator'},0},
                 logging_category = lists:duplicate(256,0),
                 base = {ptr,{struct,event_base},0},
                 hostname = lists:duplicate(256,0),
                 portnum = lists:duplicate(256,0),
                 addrinfo = {ptr,{struct,addrinfo},0}}.


