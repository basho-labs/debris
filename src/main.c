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

#include "uthash.h"


#define RIAK_GET_P 1000
#define RIAK_GET_PR 1001


#define riak_get_param_p_set(_p, p_val) riak_add_uint32_param(&_p, RIAK_GET_P, p_val);
#define riak_get_param_pr_set(_p, pr_val) riak_add_uint32_param(&_p, RIAK_GET_PR, pr_val);
#define riak_get_param_p_get(_p, p_val) riak_get_uint32_param_value(&_p, RIAK_GET_P, p_val);
#define riak_get_param_pr_get(_p, p_val) riak_get_uint32_param_value(&_p, RIAK_GET_PR, p_val);
 
int main (int argc, char *argv[])
{

  struct riak_param *params = RIAK_EMPTY_PARAMS;
  /*
  riak_add_uint32_param(&params, RIAK_GET_P, 10);
  riak_add_uint32_param(&params, RIAK_GET_PR, 20);
  */
  // the above, as a macro, looks like this:
  riak_get_param_p_set(params, 3);
  riak_get_param_pr_set(params, 2);

  int count = HASH_COUNT(params);
  printf("Param count = %d\n", count);

  uint32_t param_P = 0;
  uint32_t param_PR = 0;

  riak_get_uint32_param_value(&params, RIAK_GET_P, &param_P);
  riak_get_uint32_param_value(&params, RIAK_GET_PR, &param_PR);
  printf("P  = %d\n", param_P);
  printf("PR = %d\n", param_PR);

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
