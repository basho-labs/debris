#include "riak.h"
#include "riak_pb_proto.h"
#include <stdio.h>

void get_cb(void *protocol_data,
            void *raw_response_data,
            riak_response_callback user_cb) {
  printf("get callback\n");
  user_cb(0);
}

int get(void *protocol_data,
        riak_protocol_get_callback protocb,
        struct riak_binary *bucket,
        struct riak_binary *key,
        struct riak_get_options* options,
        riak_response_callback user_cb) {
  printf("get!\n");
  protocb(protocol_data, 0, user_cb);
  return 0;
}




// just for play, COPY ALL THE BITS!!
struct riak_protocol setup_riak_pb_proto() {
  struct riak_protocol pb;
  pb.protocol_data = 0;
  pb.get_impl = get;
  pb.get_callback = get_cb;
  return pb;
}
