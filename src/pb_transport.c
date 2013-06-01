#include "pb_transport.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <netdb.h>
#include <netinet/in.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>

#define MAXDATASIZE 100
#define GET_TD struct riak_pb_default_transport_data *td = (struct riak_pb_default_transport_data*)transport_data

void riak_default_transport(struct riak_pb_transport *t) {
  struct riak_pb_default_transport_data *td =
    (struct riak_pb_default_transport_data*) malloc(sizeof(struct riak_pb_default_transport_data));

  t->transport_data = (void*)td;
  t->connect = default_connect;
  t->disconnect = default_disconnect;
  t->send_message = default_send_message;
  t->receive_message = default_receive_message;
  t->receive_message_streamed = default_receive_message_streamed;
}


int default_connect(void* transport_data, char* ip, int port) {
  printf("Connecting to Riak\n");
  // http://beej.us/guide/bgnet/output/html/singlepage/bgnet.html#simpleclient
  int sockfd;
    struct addrinfo hints, *servinfo, *p;
    int rv;

    memset(&hints, 0, sizeof hints);
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;

    if ((rv = getaddrinfo("127.0.0.1", "10017", &hints, &servinfo)) != 0) {
        fprintf(stderr, "getaddrinfo: %s\n", gai_strerror(rv));
        return 1;
    }

    // loop through all the results and connect to the first we can
    for(p = servinfo; p != NULL; p = p->ai_next) {
        if ((sockfd = socket(p->ai_family, p->ai_socktype,
                p->ai_protocol)) == -1) {
            perror("client: socket");
            continue;
        }

        if (connect(sockfd, p->ai_addr, p->ai_addrlen) == -1) {
            close(sockfd);
            perror("client: connect");
            continue;
        }

        break;
    }

    if (p == NULL) {
        fprintf(stderr, "client: failed to connect\n");
        return 2;
    }

    freeaddrinfo(servinfo); // all done with this structure

  printf("Connected!\n");

  struct riak_pb_default_transport_data *td =
    (struct riak_pb_default_transport_data*)transport_data;
  td->socket_fd = sockfd;
  // TODO: error handling
  return 0;
}

int default_send_message(void* transport_data, struct pb_request *req) {
  GET_TD;
  uint32_t pb_len = req->msglength + 1;
  uint32_t netlen = htonl(pb_len);
  // for the (void*) in send
  uint32_t msgid = req->reqid;
  // TODO: clean up, single send etc
  // http://docs.basho.com/riak/1.1.4/references/apis/protocol-buffers/
  int s1 = send(td->socket_fd, &netlen, 4, 0);
  int s2 = send(td->socket_fd, &msgid, 1, 0);
  int s3 = send(td->socket_fd, req->reqdata, req->msglength, 0);
  // TODO: error handling
  return 0;
}

int default_receive_message(void *transport_data, struct pb_response *resp) {
  GET_TD;
  uint32_t resplen = 0;
  uint8_t  respid = 0;

  int count = recv(td->socket_fd, &resplen, 4, 0);       // length is 4 bytes
  if(resplen > 0) {
    recv(td->socket_fd, (void*)&respid, 1, 0);        // 1 byte for the response code
    uint32_t encoded_msg_length = ntohl(resplen) - 1; // -1 for the length byte
    char* buf = malloc(encoded_msg_length);
    bzero(buf, encoded_msg_length);
    recv(td->socket_fd, buf, encoded_msg_length, 0);
    resp->msglength = encoded_msg_length;
    resp->actual_respid = respid;
    resp->respdata = buf;
    // TODO: error handling
    return 0;
  } else {
    return -1;
  }
}

int default_receive_message_streamed() {
  return 0;
}

int default_disconnect(void* transport_data) {
  // need void** here
  GET_TD;
  close(td->socket_fd);
  printf("Disconnecting from Riak\n");
  // TODO: error handling
  return 0;
}

void riak_pb_connect(struct riak_pb_transport* t,
                                  struct riak_protocol* p,
                                  char* ip,
                                  int port) {
  t->connect(t->transport_data, ip, port);
}


