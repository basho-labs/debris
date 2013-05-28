/*
 * -------------------------------------------------------------------
 * riak-c-client
 *
 * Copyright (c) 2013 Dave Parfitt
 *
 * This file is provided to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file
 * except in compliance with the License.  You may obtain
 * a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 * -------------------------------------------------------------------
 */


#ifndef RIAK_H
#define RIAK_H
#include <arpa/inet.h>

struct riak_protocol {
  // protobuffs OR http
  // fn pointers to implementation of each riak feature (ping, get, put)
};

struct riak_context {
  struct riak_protocol *proto;
  // anything else... network options etc
};


// let the dev worry about string length
struct riak_string {
  char* data;
  size_t len;
};




void riak_ping(struct riak_context*);

void riak_get(struct riak_context*);

void riak_server_info(struct riak_context*);

void riak_fetch(struct riak_context*);

void riak_store(struct riak_context*);

void riak_delete(struct riak_context*);

void riak_bucket_set_props(struct riak_context*);

void riak_bucket_get_props(struct riak_context*);

void riak_list_buckets(struct riak_context*);

void riak_list_keys(struct riak_context*);

void riak_set_client_id(struct riak_context*);

void riak_get_client_id(struct riak_context*);

void riak_query_2i(struct riak_context*);

void riak_map_reduce(struct riak_context*);

#endif
