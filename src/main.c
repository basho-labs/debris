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
   // so the IP and port are really ignored
   riak_pb_connect(&riak, 0, "127.0.0.1", 10017);


   // TODO: create a single file with a list of error messages
   if(riak_pb_ping(&riak) == 0) {
     printf("PONG\n");
   }


   // test set_client_id
   // do we need to count the \0?
   struct riak_binary *client_id = new_riak_binary(6, "Foobar");
   riak_pb_set_client_id(&riak, client_id);
   free_riak_binary(client_id);

   // test get_client_id
   riak_pb_get_client_id(&riak);

   // basic PUT test
   {
     struct riak_object *o = new_riak_object();
     char *bucket = "CTestBucket";
     char *key    = "CTestKey";
     char *value  = "CTestValue";

     o->bucket.len = strlen(bucket);
     o->bucket.data = bucket;

     o->key.len = strlen(key);
     o->key.data = key;

     o->value.len = strlen(value);
     o->value.data = value;

     struct riak_response *response = new_riak_response();
     riak_pb_put(&riak,
         o,
         0,
         response);
     free(response);
   }


   // basic GET test
   int i;
   for(i = 0; i < 1000; i++) {
     struct riak_binary *bucket = new_riak_binary(3, "Foo");
     struct riak_binary *key = new_riak_binary(3, "Bar");
     struct riak_response* response = new_riak_response();
     struct riak_get_options opts;
     riak_pb_get(&riak, bucket, key, &opts, response);
     free_riak_binary(bucket);
     free_riak_binary(key);
     free_riak_response(response);
   }
   printf("GET %i objects successful\n", i);

   riak.disconnect(riak.transport_data);
   free(riak.transport_data);

   exit(0);
}
