#include "rcc.h"
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


int riak_get_uint32_param_value(struct riak_param **params, int param_id, uint32_t *val) {
  struct riak_param *p = NULL;
  HASH_FIND_INT(*params, &param_id, p);
  if(p != NULL) {
    if(p->t == RIAK_PARAM_UINT32) {
      *val = p->uint32_val;
    } else {
      return -2; // TODO: invalid param type error
    }
  } else {
    return -1; // TODO: empty param error
  }
  return 0;
}

int riak_get_bool_param_value(struct riak_param **params, int param_id, riak_boolean *val) {
  struct riak_param *p = NULL;
  HASH_FIND_INT(*params, &param_id, p);
  if(p != NULL) {
    if(p->t == RIAK_PARAM_BOOL) {
      *val = p->bool_val;
    } else {
      return -2; // TODO: invalid param type error
    }
  } else {
    return -1; // TODO: empty param error
  }
  return 0;
}

int riak_get_binary_param_value(struct riak_param **params, int param_id, struct riak_binary *val) {
  struct riak_param *p = NULL;
  HASH_FIND_INT(*params, &param_id, p);
  if(p != NULL) {
    if(p->t == RIAK_PARAM_BIN) {
      val = p->bin_val;
    } else {
      return -2; // TODO: invalid param type error
    }
  } else {
    return -1; // TODO: empty param error
  }
  return 0;
}

struct riak_param* riak_new_uint32_param(int param_id, uint32_t val) {
  struct riak_param *p = malloc(sizeof(struct riak_param));
  p->id = param_id;
  p->t = RIAK_PARAM_UINT32;
  p->uint32_val = val;
  return p;
}

// internal function
void maybe_delete_param(struct riak_param **params, int param_id) {
  struct riak_param *p = NULL;
  HASH_FIND_INT(*params, &param_id, p);
  if(p != NULL) {
    HASH_DEL(*params, p);
    printf("TODO: make this free() work correctly!!\n");
    free(p);
  }
}

void riak_add_uint32_param(struct riak_param **params, int param_id, uint32_t val) {
  struct riak_param *p = NULL;
  maybe_delete_param(params, param_id);
  p = riak_new_uint32_param(param_id, val);
  HASH_ADD_INT(*params, id, p);
}

struct riak_param* riak_new_boolean_param(int param_id, riak_boolean val) {
  struct riak_param *p = malloc(sizeof(struct riak_param));
  p->id = param_id;
  p->t = RIAK_PARAM_BOOL;
  p->bool_val = val;
  return p;
}

void riak_add_boolean_param(struct riak_param** params, int param_id, riak_boolean val) {
  struct riak_param *p = NULL;
  maybe_delete_param(params, param_id);
  p = riak_new_boolean_param(param_id, val);
  HASH_ADD_INT(*params, id, p);
}

struct riak_param* riak_new_binary_param(int param_id, struct riak_binary *val) {
  struct riak_param *p = malloc(sizeof(struct riak_param));
  p->id = param_id;
  p->t = RIAK_PARAM_BIN;
  p->bin_val = val;
  return p;
}

void riak_add_binary_param(struct riak_param** params, int param_id, struct riak_binary *val) {
  struct riak_param *p = NULL;
  maybe_delete_param(params, param_id);
  p = riak_new_binary_param(param_id, val);
  HASH_ADD_INT(*params, id, p);
}

void riak_get(struct riak_context *ctx,
              struct riak_binary *bucket,
              struct riak_binary *key,
              struct riak_get_options* opts,
              riak_response_callback user_cb) {

  struct riak_protocol *proto = ctx->proto;
  proto->get_impl(proto->protocol_data,
                  proto->get_callback,
                  bucket,
                  key,
                  opts,
                  user_cb);
}

