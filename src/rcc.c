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


int get_uint32_param_value(struct riak_param* p, uint32_t *val) {
  if(p != 0) {
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

int get_bool_param_value(struct riak_param* p, riak_boolean *val) {
  if(p != 0) {
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

int get_binary_param_value(struct riak_param* p, struct riak_binary *val) {
  if(p != 0) {
    if(p->t == RIAK_PARAM_UINT32) {
      val = p->bin_val;
    } else {
      return -2; // TODO: invalid param type error
    }
  } else {
    return -1; // TODO: empty param error
  }
  return 0;
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

