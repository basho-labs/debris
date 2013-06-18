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

int riak_get(struct riak_context *ctx,
             struct riak_binary *bucket,
             struct riak_binary *key,
             struct riak_get_options *get_options,
             riak_response_callback resonse_cb) {
  return 0;
}

