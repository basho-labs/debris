/*********************************************************************
 *
 * riak_types.h: Riak C Client Types
 *
 * Copyright (c) 2007-2013 Basho Technologies, Inc.  All Rights Reserved.
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
 *
 *********************************************************************/

#ifndef RIAK_H
#define RIAK_H

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <arpa/inet.h>
#include <event2/dns.h>
#include <event2/bufferevent.h>
#include <event2/buffer.h>
#include <event2/util.h>
#include <event2/event.h>
#include "riak_types.h"
#include "riak.pb-c.h"
#include "riak_kv.pb-c.h"
#include "riak_context.h"

// TOP level structures

// Based off of ProtobufCBinaryData
typedef struct _riak_binary {
    riak_size_t   len;
    riak_uint8_t *data;
} riak_binary;

#include "riak_object.h"

// Based on RpbGetResp
typedef struct _riak_get_response {
    riak_boolean_t has_vclock;
    riak_binary    vclock;
    riak_boolean_t has_unmodified;
    riak_boolean_t unmodified;
    riak_boolean_t deleted;
    riak_int32_t   n_content;
    riak_object   *content;
} riak_get_response;

typedef struct _riak_get_options {
    riak_boolean_t has_r;
    riak_uint32_t  r;
    riak_boolean_t has_pr;
    riak_uint32_t  pr;
    riak_boolean_t has_basic_quorum;
    riak_boolean_t basic_quorum;
    riak_boolean_t has_notfound_ok;
    riak_boolean_t notfound_ok;
    riak_boolean_t has_if_modified;
    riak_binary    if_modified;
    riak_boolean_t has_head;
    riak_boolean_t head;
    riak_boolean_t has_deletedvclock;
    riak_boolean_t deletedvclock;
    riak_boolean_t has_timeout;
    riak_uint32_t  timeout;
    riak_boolean_t has_sloppy_quorum;
    riak_boolean_t sloppy_quorum;
    riak_boolean_t has_n_val;
    riak_uint32_t  n_val;
} riak_get_options;

typedef struct _riak_put_response {
    riak_uint32_t  n_content;
    riak_object   *content;
    riak_boolean_t has_vclock;
    riak_binary    vclock;
    riak_boolean_t has_key;
    riak_binary    key;
} riak_put_response;

typedef struct _riak_put_options {
    // KEY is in riak_object, so is this copy needed?
    //riak_boolean_t has_key;
    //riak_binary    key;
    riak_boolean_t has_vclock;
    riak_binary    vclock;
    //RpbContent *content;
    riak_boolean_t has_w;
    riak_uint32_t  w;
    riak_boolean_t has_dw;
    riak_uint32_t  dw;
    riak_boolean_t has_return_body;
    riak_boolean_t return_body;
    riak_boolean_t has_pw;
    riak_uint32_t  pw;
    riak_boolean_t has_if_not_modified;
    riak_boolean_t if_not_modified;

    riak_boolean_t has_if_none_match;
    riak_boolean_t if_none_match;
    riak_boolean_t has_return_head;
    riak_boolean_t return_head;
    riak_boolean_t has_timeout;
    riak_uint32_t  timeout;
    riak_boolean_t has_asis;
    riak_boolean_t asis;
    riak_boolean_t has_sloppy_quorum;
    riak_boolean_t sloppy_quorum;
    riak_boolean_t has_n_val;
    riak_uint32_t  n_val;
} riak_put_options;


typedef struct _riak_listbuckets_response {
    riak_uint32_t  n_buckets;
    riak_binary*   buckets;
    // TODO: Keep in struct or pass as argument?
    riak_boolean_t done;
} riak_listbuckets_response;

// Based on RpbListKeysResp
typedef struct _riak_listkeys_response {
    riak_uint32_t  n_keys;
    riak_binary*   keys;
    // TODO: Keep in struct or pass as argument?
    riak_boolean_t done;
} riak_listkeys_response;


typedef struct _riak_ping_response {
// Nothing to see here
} riak_ping_response;

void write_callback(struct bufferevent *bev, void *ptr);


// basic ops
void riak_ping(riak_context*);

// we'll provide an async version
// int riak_get(riak_context*, struct riak_get_options*, riak_get_callback*);
// and a sync version that calls the async version:

typedef void (*riak_ping_response_callback)(riak_ping_response *response, void *ptr);

typedef void (*riak_get_response_callback)(riak_get_response *response, void *ptr);

typedef void (*riak_put_response_callback)(riak_put_response *response, void *ptr);

typedef void (*riak_listbuckets_response_callback)(riak_listbuckets_response *response, void *ptr);

typedef void (*riak_listkeys_response_callback)(riak_listkeys_response *response, void *ptr);

int riak_get(riak_context*,
             riak_binary *bucket,
             riak_binary *key,
             riak_get_options*,
             riak_get_response_callback);

void riak_server_info(riak_context*);

void riak_fetch(riak_context*);

void riak_store(riak_context*);

void riak_delete(riak_context*);

void riak_bucket_set_props(riak_context*);

void riak_bucket_get_props(riak_context*);

//void riak_list_buckets(riak_context*);
int riak_list_buckets(riak_context *ctx);

void riak_list_keys(riak_context*);

void riak_set_client_id(riak_context*);

void riak_get_client_id(riak_context*);

void riak_query_2i(riak_context*);

void riak_map_reduce(riak_context*);

#endif
