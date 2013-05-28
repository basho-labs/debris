#include <pthread.h>
#include <stdlib.h>
#include "riak.h"
#include "pb_messages.h"
#include <stdio.h>
#include "riak.pb-c.h"
#include "riak_kv.pb-c.h"
#include "stdlib.h"
#include "string.h"

struct riak_string* new_string(char *foo) {
   struct riak_string *s = (struct riak_string*)malloc(sizeof(struct riak_string));
   s->data = foo;
   s->len = strlen(foo);
   return s;
}

int main (int argc, char *argv[])
{
   struct riak_pb_transport riak;
   riak_default_transport(&riak);
   // ip and port are hardcoded internally right now
   riak_pb_connect(&riak, 0, "127.0.0.1", 10017);

   struct riak_string *bucket = new_string("Foo");
   struct riak_string *key = new_string("Bar");

   riak_pb_get(&riak, bucket, key);

   free(bucket);
   free(key);

   /*
   struct riak_pb_transport riak;
   riak_default_transport(&riak);
   riak_connect(&riak, 0, "127.0.0.1", 10017);
   default_send_message(riak.transport_data, 9, buf, len);
   riak.disconnect();
   */
}
