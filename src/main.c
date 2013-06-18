#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include "riak.pb-c.h"
#include "riak_kv.pb-c.h"
#include "stdlib.h"
#include "string.h"
#include <event.h>
#include <event2/event.h>
#include <inttypes.h>

#include "rcc.h"
#include "rcc_utils.h"

void foo(struct riak_response *resp) {
  printf("FOO cb!\n");
}

#include "uthash.h"


#define RIAK_GET_P 1000
#define RIAK_GET_PR 1001


int main (int argc, char *argv[])
{
/*
   struct riak_binary *bucket = new_riak_binary(3, "Foo");
   struct riak_binary *key = new_riak_binary(3, "Bar");
   struct riak_response* response = new_riak_response();
   struct riak_get_options opts;

   struct riak_protocol pb = setup_riak_pb_proto();
   struct riak_context ctx;
   ctx.proto = &pb;
   ctx.malloc_fn=malloc;
   riak_get(&ctx,
             bucket,
             key,
             &opts,
             foo);

   free_riak_binary(bucket);
   free_riak_binary(key);
   free_riak_response(response);
   // typedef void (*event_callback_fn)(evutil_socket_t, short, void *);
   //struct event_base *eb = event_base_new();
   //event_base_free(eb);
   */
   return 0;
}
