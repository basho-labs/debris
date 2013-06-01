#include "riak.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <netdb.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <arpa/inet.h>

struct riak_object *new_riak_object() {
  struct riak_object *o = (struct riak_object*)malloc(sizeof(struct riak_object));
  bzero(o, sizeof(struct riak_object));
  o->value = (struct riak_binary*)malloc(sizeof(struct riak_binary));
  return o;
}

void free_riak_object(struct riak_object *o) {
  if(o == 0) {
    return;
  }

  if(o->value != 0) {
    free(o->value);
  }
  free(o);
}


struct riak_binary *new_riak_binary(size_t len, uint8_t *data) {
  struct riak_binary *b = (struct riak_binary*)malloc(sizeof(struct riak_binary));
  b->len = len;
  b->data = (uint8_t*)malloc(len);
  memcpy(b->data, data, len);
  return b;
}

void free_riak_binary(struct riak_binary *b) {
  if(b == 0) {
    return;
  }
  free(b->data);
  free(b);
}


struct riak_response* new_riak_response() {
  struct riak_response* r = (struct riak_response*)malloc(sizeof(struct riak_response));
  bzero(r, sizeof(struct riak_response));
  return r;
}

void free_riak_response(struct riak_response *r) {
  if(r == 0) {
    return;
  }
  if(r->object_count > 0) {
    // TODO
    free(r->objects);
  }
  free(r);

}

