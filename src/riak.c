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

void riak_get(struct riak_context *ctx,
              struct riak_binary *bucket,
              struct riak_binary *key,
              struct riak_get_options* opts,
              riak_response_callback user_cb) {

  ctx->proto->get_impl(ctx->proto->protocol_data,
                  ctx->proto->get_callback,
                  bucket,
                  key,
                  opts,
                  user_cb);
}

