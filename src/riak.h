
#ifndef RIAK_H
#define RIAK_H
#include <arpa/inet.h>

struct riak_protocol {
  // protobuffs OR http
  // fn pointers to implementation of each riak feature (ping, get, put)
};

struct riak_context {
  struct riak_protocol *proto;
  // anything else... network options etc
};

// let the dev worry about string length
struct riak_string {
  char* data;
  size_t len;
};

// NOT USED
//struct pb_request {
//  uint32_t msglength;
//  uint8_t reqid;          // Protobuffs msg id for request
//  uint8_t respid;         // Expected protobuffs msg id for response
//  void* msgdata;
//};

// NOT USED
//struct pb_response {
//  uint8_t respid;
//  uint32_t len;
//  void* buf;
//};

struct riak_pb_transport {
  // socket layer
  void* transport_data;
  int (*connect)(void *transport_data, char* ip, int port);
  int (*disconnect)();
  int (*send_message)(void *transport_data, uint32_t msgid, void* data, unsigned len);
  int (*receive_message)(void *transport_data, uint32_t msgid, void**);
  int (*receive_message_chunked)();
};


// riak_context is incorrect here
struct riak_context *riak_pb_connect(struct riak_pb_transport*,
                                  struct riak_protocol*,
                                  char* ip,
                                  int port);

void riak_ping(struct riak_context*);

void riak_get(struct riak_context*);

void riak_server_info(struct riak_context*);

void riak_fetch(struct riak_context*);

void riak_store(struct riak_context*);

void riak_delete(struct riak_context*);

void riak_bucket_set_props(struct riak_context*);

void riak_bucket_get_props(struct riak_context*);

void riak_list_buckets(struct riak_context*);

void riak_list_keys(struct riak_context*);

void riak_set_client_id(struct riak_context*);

void riak_get_client_id(struct riak_context*);

void riak_query_2i(struct riak_context*);

void riak_map_reduce(struct riak_context*);

#endif
