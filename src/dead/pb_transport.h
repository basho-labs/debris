#ifndef PB_TRANSPORT_H
#define PB_TRANSPORT_H
#include "riak.h"

struct pb_request {
  uint32_t msglength;
  uint8_t reqid;          // Protobuffs msg id for request
  void *reqdata;
};

struct pb_response {
  uint32_t msglength;
  uint8_t expected_respid;
  uint8_t actual_respid;
  void *respdata;
};


struct riak_pb_transport {
  // socket layer
  void* transport_data;
  int (*connect)(void *transport_data, char* ip, int port);
  int (*disconnect)(void *transport_data);
  int (*send_message)(void *transport_data, struct pb_request*);
  int (*receive_message)(void *transport_data, struct pb_response*);
  int (*receive_message_streamed)();
};

void riak_default_transport(struct riak_pb_transport *t);

// riak_context is incorrect here
void riak_pb_connect(struct riak_pb_transport*,
                      struct riak_protocol*,
                      char* ip,
                      int port);

struct riak_pb_default_transport_data {
  int socket_fd;
};


int default_connect(void *transport_data, char* ip, int port);
int default_send_message(void *transport_data, struct pb_request* req);
int default_receive_message(void *transport_data, struct pb_response* resp);
int default_receive_message_streamed();
int default_disconnect(void *transport_data);

#endif
