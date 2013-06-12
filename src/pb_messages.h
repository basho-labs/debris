#ifndef PB_MESSAGES_H
#define PB_MESSAGES_H

#include "riak.h"
#include "pb_transport.h"


//int riak_pb_ping(struct riak_pb_transport*);

//int riak_pb_get_client_id(struct riak_pb_transport*);
//int riak_pb_set_client_id(struct riak_pb_transport*, struct riak_binary *id);

void create_get_request(struct riak_binary *bucket,
                        struct riak_binary *key,
                        struct riak_get_options*,
                        struct pb_request *request);


int riak_pb_get(struct riak_pb_transport*,
                struct riak_binary *bucket,
                struct riak_binary *key,
                struct riak_get_options*,
                struct riak_response*);

/*
int riak_pb_put(struct riak_pb_transport*,
                struct riak_object*,
                struct riak_put_options*,
                struct riak_response*);
                */
#endif
