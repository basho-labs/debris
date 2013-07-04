/*********************************************************************
 *
 * riak_types.h: Riak Utilities
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

#ifndef RIAK_UTILS_H
#define RIAK_UTILS_H

#include "riak.h"

// Why free this way?  Why not just free pointer?
#define riak_free(p) _riak_free((void**)&(p))
void _riak_free(void **p);
// helper fn's
riak_response *riak_response_new();
void riak_response_free(riak_response*);

riak_object *riak_object_new();
void riak_object_free(riak_object*);

// TODO: NOT CHARSET SAFE, need iconv

/**
 * @brief Allocate a new `riak_binary` struct
 * @param len Length of binary in bytes
 * @param data Pointer to binary data
 * @returns pointer to newly created `riak_binary` struct
 */
riak_binary *riak_binary_new(riak_size_t len, riak_uint8_t *data);

/**
 * @brief Allocate a new riak_binary and populate from data pointer
 * @param bin Existing `riak_binary` to be populated
 * @param len Length of binary in bytes
 * @param data Source of binary to be copied to bin
 */
void riak_binary_populate(riak_binary *bin, riak_size_t len, riak_uint8_t *data);

/**
 * @brief Free allocated memory used by `riak_binary`
 */
void riak_binary_free(riak_binary *bin);
void riak_binary_copy(riak_binary* to, riak_binary* from);
void riak_binary_deep_copy(riak_binary *to, riak_binary *from);

void eventcb(struct bufferevent *bev, short events, void *ptr);
/**
 * @brief Send PB message via bufferevent
 * @param ctx Context of request
 * @param reqid Request Identifier
 * @param msgbuf Packed binary request
 * @param len Length of `msgbuf`
 */
int riak_send_req(riak_context *ctx, riak_uint8_t reqid, riak_uint8_t *msgbuf, riak_size_t len);

#endif
