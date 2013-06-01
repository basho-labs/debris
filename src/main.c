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
   struct riak_object *o = new_riak_object();
   o->value = new_riak_binary(3,"Foo");
   free_riak_object(o);

   struct riak_pb_transport riak;
   riak_default_transport(&riak);
   // ip and port are hardcoded internally right now
   riak_pb_connect(&riak, 0, "127.0.0.1", 10017);
   int i = 0;
   {
     printf("i = %d\n", i);
     struct riak_binary *bucket = new_riak_binary(3, "Foo");
     struct riak_binary *key = new_riak_binary(3, "Bar");
     struct riak_response* response = (struct riak_response*)malloc(sizeof(struct riak_response));
     riak_pb_get(&riak, bucket, key, response);
     printf("Response object count = %d\n", response->object_count);
     printf("[%s]\n", response->objects[0].value->data);
     free_riak_binary(bucket);
     free_riak_binary(key);
     free(response);
   }

    i++;
   {
     printf("i = %d\n", i);
     struct riak_binary *bucket = new_riak_binary(3, "Foo");
     struct riak_binary *key = new_riak_binary(3, "Bar");
     struct riak_response* response = (struct riak_response*)malloc(sizeof(struct riak_response));
     riak_pb_get(&riak, bucket, key, response);
     printf("Response object count = %d\n", response->object_count);
     printf("[%s]\n", response->objects[0].value->data);
     free_riak_binary(bucket);
     free_riak_binary(key);
     free(response);
   }



   /*
   struct riak_pb_transport riak;
   riak_default_transport(&riak);
   riak_connect(&riak, 0, "127.0.0.1", 10017);
   default_send_message(riak.transport_data, 9, buf, len);
   riak.disconnect();
   */
}
