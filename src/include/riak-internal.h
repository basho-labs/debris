/*********************************************************************
 *
 * riak-internal.h: Riak C Client Types
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

#ifndef RIAK_INTERNAL_H
#define RIAK_INTERNAL_H

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <string.h>
#include <arpa/inet.h>
#include <event2/dns.h>
#include <event2/bufferevent.h>
#include <event2/buffer.h>
#include <event2/util.h>
#include <event2/event.h>
#include "riak_types.h"
#include "riak.pb-c.h"
#include "riak_kv.pb-c.h"
#include "riak_error.h"
#include "riak_context.h"
#include "riak_event.h"
#include "riak_log.h"
#include "riak_binary-internal.h"
#include "riak_object.h"

// Based on RpbGetServerInfoResp
struct _riak_serverinfo_response
{
    riak_boolean_t        has_node;
    riak_binary           node;
    riak_boolean_t        has_server_version;
    riak_binary           server_version;

    RpbGetServerInfoResp *_internal;
};

// Based on RpbGetResp
struct _riak_get_response {
    riak_boolean_t has_vclock;
    riak_binary    vclock;
    riak_boolean_t has_unmodified;
    riak_boolean_t unmodified;
    riak_boolean_t deleted;
    riak_int32_t   n_content;
    riak_object  **content; // Array of pointers to allow expansion

    RpbGetResp    *_internal;
};

// Based on RpbGetReq
struct _riak_get_options {
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
};

// Based on RpbPutResp
struct _riak_put_response {
    riak_uint32_t  n_content;
    riak_object  **content; // Array of pointers to match Get
    riak_boolean_t has_vclock;
    riak_binary    vclock;
    riak_boolean_t has_key;
    riak_binary    key;

    RpbPutResp   *_internal;
};

// Based on RpbPutReq
struct _riak_put_options {
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
};


// Based on RpbListBucketsResp
struct _riak_listbuckets_response {
    riak_uint32_t        n_buckets;
    riak_binary        **buckets; // Array of pointers to allow growth
    riak_boolean_t       done;

    riak_uint32_t        n_responses;
    RpbListBucketsResp **_internal; // Array for many responses
};

// Based on RpbListKeysResp
struct _riak_listkeys_response {
    riak_uint32_t     n_keys;
    riak_binary     **keys; // Array of pointers to allow growth
    riak_boolean_t    done;

    riak_uint32_t     n_responses;
    RpbListKeysResp **_internal; // Array for many responses
};

// Based on RpbErrorResp
struct _riak_error_response {
    riak_uint32_t errcode;
    riak_binary   errmsg;

    RpbErrorResp *_internal;
};

struct _riak_ping_response {
    riak_boolean_t success;
};

// Based on RpbDelReq
struct _riak_delete_options
{
    riak_boolean_t has_vclock;
    riak_binary    vclock;
    riak_boolean_t has_w;
    riak_uint32_t  w;
    riak_boolean_t has_dw;
    riak_uint32_t  dw;
    riak_boolean_t has_pw;
    riak_uint32_t  pw;
    riak_boolean_t has_timeout;
    riak_uint32_t  timeout;
    riak_boolean_t has_sloppy_quorum;
    riak_boolean_t sloppy_quorum;
    riak_boolean_t has_n_val;
    riak_uint32_t  n_val;
};

struct _riak_delete_response {
// Nothing to see here
};

// Based on RpbGetClientIdResp
struct _riak_get_clientid_response
{
    riak_binary client_id;

    RpbGetClientIdResp *_internal;
};

// Based on RpbSetClientIdReq
struct _riak_get_clientid_request
{
    riak_binary client_id;
};

// Placeholder
struct _riak_set_clientid_response
{
// Empty
};

#endif
