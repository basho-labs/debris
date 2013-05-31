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
  size_t len;
  char* data;
};

// protobuf-c maps boolean to an int
typedef int riak_boolean;

// TODO: riak_string vs riak_binary
struct riak_binary {
  size_t len;
  uint8_t *data;
};

struct riak_object {
  struct riak_binary *value;

  riak_boolean has_charset;
  struct riak_binary *charset;

  riak_boolean has_last_mod;
  uint32_t last_mod;

  riak_boolean has_last_mod_usecs;
  uint32_t last_mod_usecs;

  riak_boolean has_content_type;
  struct riak_binary *content_type;

  riak_boolean has_content_encoding;
  struct riak_binary *encoding;

  riak_boolean has_deleted;
  riak_boolean deleted;

  riak_boolean has_vtag;
  struct riak_binary *vtag;

  size_t n_links;
  //RpbLink **links;

  size_t n_usermeta;
  //RpbPair **usermeta;
  size_t n_indexes;
  //RpbPair **indexes;
};

struct riak_vclock {

};

struct riak_response {
  struct riak_vclock *vclock;
  //struct riak_object objects[];
  riak_boolean unmodified;
  riak_boolean deleted;
};


// helper fn's
struct riak_response *new_riak_response();
void free_riak_response(struct riak_response*);

struct riak_object *new_riak_object();
void free_riak_object(struct riak_object*);

struct riak_binary *new_riak_binary(size_t len, uint8_t *data);
void free_riak_binary(struct riak_binary*);



// basic ops
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
