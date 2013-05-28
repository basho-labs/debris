#ifndef RIAK_H
#define RIAK_H
#include <arpa/inet.h>

typedef enum {RIAK_PROTO_HTTP,
              RIAK_PROTO_PB} riak_proto;



struct riak_protocol {
  // protocol *implementation*
  int bar;
};

struct riak_context {
  int foo;
};

// let the dev worry about string length
struct riak_string {
  char* data;
  size_t len;
};

struct pb_request {
  uint32_t msglength;
  uint8_t reqid;          // Protobuffs msg id for request
  uint8_t respid;         // Expected protobuffs msg id for response
  void* msgdata;
};

struct pb_response {
  uint8_t respid;
  uint32_t len;
  void* buf;
};

struct riak_pb_transport {
  // socket layer
  void* transport_data;
  int (*connect)(void *transport_data, char* ip, int port);
  int (*disconnect)();
  int (*send_message)(void *transport_data, uint32_t msgid, void* data, unsigned len);
  int (*receive_message)(void *transport_data, uint32_t msgid, void**);
  int (*receive_message_chunked)();
};

struct riak_context *riak_pb_connect(struct riak_pb_transport*,
                                  struct riak_protocol*,
                                  char* ip,
                                  int port);

void riak_ping();

void riak_get();

void riak_server_info();

void riak_fetch();

void riak_store();

void riak_delete();

void riak_bucket_set_props();

void riak_bucket_get_props();

void riak_list_buckets();

void riak_list_keys();

void riak_set_client_id();

void riak_get_client_id();

void riak_query_2i();

void riak_map_reduce();

#endif
