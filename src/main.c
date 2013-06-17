#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include "riak.pb-c.h"
#include "riak_kv.pb-c.h"
#include "stdlib.h"
#include "string.h"
#include <event.h>
#include <event2/event.h>

#include "rcc.h"
#include "rcc_pb_proto.h"
#include "rcc_utils.h"

void foo(struct riak_response *resp) {
  printf("FOO cb!\n");
}

int main (int argc, char *argv[])
{
  struct riak_param p;
  p.id = 1;
  p.t = RIAK_PARAM_UINT32;
  p.uint32_val = 1000;

  struct riak_param p2;
  p2.id = 2;
  p2.t = RIAK_PARAM_BOOL;
  p2.bool_val = RIAK_TRUE;

  uint32_t ui = 0;
  int ret = get_uint32_param_value(&p2, &ui);
  printf("VAL = %d\n", ui);
  printf("RET = %d\n", ret);

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
