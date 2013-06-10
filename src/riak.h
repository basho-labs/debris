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


#define riak_free(p) _riak_free((void**)&(p))
void _riak_free(void **p);

struct riak_protocol;

// TOP level structures
typedef struct riak_context {
  struct riak_protocol *proto;
  // anything else... network options etc
} RiakContext;


// protobuf-c maps boolean to an int
typedef int riak_boolean;

typedef struct riak_binary {
  size_t len;
  uint8_t *data;
} RiakBinary;

struct riak_object {
  struct riak_binary bucket;

  riak_boolean has_key;
  struct riak_binary key;

  struct riak_binary value;

  riak_boolean has_charset;
  struct riak_binary charset;

  riak_boolean has_last_mod;
  uint32_t last_mod;

  riak_boolean has_last_mod_usecs;
  uint32_t last_mod_usecs;

  riak_boolean has_content_type;
  struct riak_binary content_type;

  riak_boolean has_content_encoding;
  struct riak_binary encoding;

  riak_boolean has_deleted;
  riak_boolean deleted;

  riak_boolean has_vtag;
  struct riak_binary vtag;

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
  riak_boolean unmodified;
  riak_boolean deleted;
  int object_count;
  struct riak_object *objects;
};


struct riak_get_options {
  riak_boolean has_r;
  uint32_t r;
  riak_boolean has_pr;
  uint32_t pr;
  riak_boolean has_basic_quorum;
  riak_boolean basic_quorum;
  riak_boolean has_notfound_ok;
  riak_boolean notfound_ok;
  riak_boolean has_if_modified;
  struct riak_binary  if_modified;
  riak_boolean has_head;
  riak_boolean head;
  riak_boolean has_deletedvclock;
  riak_boolean deletedvclock;
  riak_boolean has_timeout;
  uint32_t timeout;
  riak_boolean has_sloppy_quorum;
  riak_boolean sloppy_quorum;
  riak_boolean has_n_val;
  uint32_t n_val;
};


struct riak_put_options {
  riak_boolean has_key;
  struct riak_binary key;
  riak_boolean has_vclock;
  struct riak_binary vclock;
  //RpbContent *content;
  riak_boolean has_w;
  uint32_t w;
  riak_boolean has_dw;
  uint32_t dw;
  riak_boolean has_return_body;
  riak_boolean return_body;
  riak_boolean has_pw;
  uint32_t pw;
  riak_boolean has_if_not_modified;
  riak_boolean if_not_modified;
  riak_boolean has_if_none_match;
  riak_boolean if_none_match;
  riak_boolean has_return_head;
  riak_boolean return_head;
  riak_boolean has_timeout;
  uint32_t timeout;
  riak_boolean has_asis;
  riak_boolean asis;
  riak_boolean has_sloppy_quorum;
  riak_boolean sloppy_quorum;
  riak_boolean has_n_val;
  uint32_t n_val;
};


// RIAK_PROTOCOL
typedef void (*riak_proto_get_callback)(void *protocol_data,
                                        void *data,
                                        struct riak_response*);

typedef void (*riak_proto_put_callback)(void *protocol_data,
                                        void *data,
                                        struct riak_response*);


typedef int (*riak_protocol_get)(void *protocol_data,
                              struct riak_binary *bucket,
                              struct riak_binary *key,
                              struct riak_get_options*,
                              riak_proto_get_callback);

typedef int (*riak_protocol_put)(void *protocol_data,
                              struct riak_binary *bucket,
                              struct riak_binary *key,
                              struct riak_binary *value,
                              struct riak_put_options*,
                              riak_proto_put_callback);



struct riak_protocol {
  void *protocol_data; // passed as the first param to each op
  riak_protocol_get get_impl;
  riak_protocol_put put_impl;
};


// helper fn's
struct riak_response *new_riak_response();
void free_riak_response(struct riak_response*);

struct riak_object *new_riak_object();

void free_riak_object(struct riak_object*);

// TODO: NOT CHARSET SAFE, need iconv
struct riak_binary *new_riak_binary(size_t len, char *data);
void populate_riak_binary(struct riak_binary *b, size_t len, uint8_t *data);
void free_riak_binary(struct riak_binary*);



// basic ops
void riak_ping(struct riak_context*);

// we'll provide an async version
// int riak_get(struct riak_context*, struct riak_get_options*, riak_get_callback*);
// and a sync version that calls the async version:
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
