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
  o->has_content_type = 0;
  o->has_charset = 0;
  o->has_content_encoding = 0;
  o->has_vtag = 0;
  o->has_last_mod = 0;
  o->has_last_mod_usecs = 0;
  o->value = (struct riak_binary*)malloc(sizeof(struct riak_binary));
  o->content_type = 0;
  o->encoding = 0;
  o->vtag = 0;
  o->n_links = 0;
  //RpbLink **links;
  o->last_mod = 0;
  o->last_mod_usecs = 0;
  o->n_usermeta = 0;
  //RpbPair **usermeta;
  o->n_indexes = 0;
  //RpbPair **indexes;
  o->has_deleted = 0;
  o->deleted = 0;
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
  b->data = data;
  return b;
}

void free_riak_binary(struct riak_binary *b) {
  if(b == 0) {
    return;
  }
  free(b->data);
  free(b);
}


