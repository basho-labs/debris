#include "rcc_utils.h"
#include <stdlib.h>
#include <string.h>

void _riak_free(void **pp) {
  if(pp != NULL && *pp != NULL) {
    free(*pp);
    *pp = NULL;
  }
}

struct riak_object *new_riak_object() {
  struct riak_object *o = (struct riak_object*)malloc(sizeof(struct riak_object));
  // TODO: do I need bzero?
  bzero(o, sizeof(struct riak_object));
  return o;
}

void free_riak_object(struct riak_object *o) {
  if(o == 0) {
    return;
  }
  // TODO
  riak_free(o);
}


// be careful
struct riak_binary *new_riak_binary(size_t len, char *data) {
  struct riak_binary *b = (struct riak_binary*)malloc(sizeof(struct riak_binary));
  b->len = len;
  b->data = (uint8_t*)malloc(len);
  memcpy(b->data, data, len);
  return b;
}

void populate_riak_binary(struct riak_binary *b, size_t len, uint8_t *data) {
  b->len = len;
  b->data = (uint8_t*)malloc(len);
  memcpy(b->data, data, len);
}

void free_riak_binary(struct riak_binary *b) {
  if(b == 0) {
    return;
  }
  riak_free(b->data);
  riak_free(b);
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
    //for(i = 0; i < r->object_count; i++) {
    //  free_riak_object(&r->objects[i]);
    //}
  }
  riak_free(r);

}


