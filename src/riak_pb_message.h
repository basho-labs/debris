/*********************************************************************
 *
 * riak_pb_message.h: Riak C Client Protocol Buffer Message
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

#ifndef RIAK_PB_MESSAGE_H
#define RIAK_PB_MESSAGE_H

#include "riak.h"
#include "riak_kv.pb-c.h"

#define MSG_RPBERRORRESP 0

// 0 length
#define MSG_RPBPINGREQ 1

// 0 length
#define MSG_RPBPINGRESP 2

#define MSG_RPBGETCLIENTIDREQ 3
#define MSG_RPBGETCLIENTIDRESP 4
#define MSG_RPBSETCLIENTIDREQ 5
#define MSG_RPBSETCLIENTIDRESP 6
#define MSG_RPBGETSERVERINFOREQ 7
#define MSG_RPBGETSERVERINFORESP 8
#define MSG_RPBGETREQ 9
#define MSG_RPBGETRESP 10
#define MSG_RPBPUTREQ 11

// 0 length
#define MSG_RPBPUTRESP 12

#define MSG_RPBDELREQ 13
#define MSG_RPBDELRESP 14
#define MSG_RPBLISTBUCKETSREQ 15
#define MSG_RPBLISTBUCKETSRESP 16
#define MSG_RPBLISTKEYSREQ 17

// streaming
#define MSG_RPBLISTKEYSRESP 18

#define MSG_RPBGETBUCKETREQ 19
#define MSG_RPBGETBUCKETRESP 20
#define MSG_RPBSETBUCKETREQ 21
#define MSG_RPBSETBUCKETRESP 22
#define MSG_RPBMAPREDREQ 23

// streaming
#define MSG_RPBMAPREDRESP 24

#define MSG_RPBINDEXREQ 25
#define MSG_RPBINDEXRESP 26
#define MSG_RPBSEARCHQUERYREQ 27
#define MSG_RBPSEARCHQUERYRESP 28

typedef struct _riak_pb_request {
    riak_uint32_t msglength;
    riak_uint8_t reqid;          // Protobuffs msg id for request
    riak_uint8_t *reqdata;
} riak_pb_request;

typedef struct _riak_pb_response {
    uint32_t msglength;
    uint8_t response_id; // TODO: need to do something for the error state
    riak_uint8_t *respdata;
} riak_pb_response;

int pb_encode_get_request(riak_context*,
                          riak_binary*,
                          riak_binary*,
                          riak_get_options*,
                          riak_pb_request*);

int pb_decode_get_response(riak_context*,
                           riak_pb_response*,
                           riak_response*);

#endif
