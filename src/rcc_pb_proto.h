#ifndef RIAK_PB_PROTO_H
#define RIAK_PB_PROTO_H
#include "rcc.h"

// TODO: probably need user_data as a void*
// TODO: do bolth get fn and callback need to return an int?


// TODO: not the best way to go about this
// just for play, COPY ALL THE BITS!!

void get_cb(void *protocol_data,
            void *raw_response_data,
            riak_response_callback);

int get(void *protocol_data,
        riak_protocol_get_callback protocb,
        struct riak_binary *bucket,
        struct riak_binary *key,
        struct riak_get_options*,
        riak_response_callback);


#endif
