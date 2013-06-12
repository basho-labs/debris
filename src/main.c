#include <pthread.h>
#include <stdlib.h>
#include "riak.h"
#include "riak_pb_proto.h"
#include "riak_utils.h"
#include "pb_messages.h"
#include <stdio.h>
#include "riak.pb-c.h"
#include "riak_kv.pb-c.h"
#include "stdlib.h"
#include "string.h"

void foo(struct riak_response *resp) {
  printf("FOO cb!\n");
}

int main (int argc, char *argv[])
{
   struct riak_binary *bucket = new_riak_binary(3, "Foo");
   struct riak_binary *key = new_riak_binary(3, "Bar");
   struct riak_response* response = new_riak_response();
   struct riak_get_options opts;

   struct riak_protocol pb = setup_riak_pb_proto();
   struct riak_context ctx;
   ctx.proto = &pb;

   riak_get(&ctx,
             bucket,
             key,
             &opts,
             foo);

   free_riak_binary(bucket);
   free_riak_binary(key);
   free_riak_response(response);


   exit(0);
}
