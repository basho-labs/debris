#ifndef PB_TRANSPORT_H
#define PB_TRANSPORT_H
#include "riak.h"
void riak_default_transport(struct riak_pb_transport *t);

struct riak_pb_default_transport_data {
  int socket_fd;
};

int default_connect(void *transport_data, char* ip, int port);
int default_send_message(void *transport_data, uint32_t msgid, void* data, unsigned len);
int default_receive_message(void *transport_data, uint32_t msgid, void**);
int default_receive_message_chunked();
int default_disconnect();

#endif
