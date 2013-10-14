/*********************************************************************
 *
 * riak_binary-internal.h: Riak C Client Binary Object Utilities
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

#include "riak_kv.pb-c.h"
#include "riak.pb-c.h"

#ifndef RIAK_BINARY_INTERNAL_H_
#define RIAK_BINARY_INTERNAL_H_

// Based off of ProtobufCBinaryData
struct _riak_binary {
    riak_size_t   len;
    riak_uint8_t *data;
};

void
riak_binary_to_pb_copy_ptr(ProtobufCBinaryData *to,
                           riak_binary         *from);
#define riak_binary_to_pb_copy(A,B) riak_binary_to_pb_copy_ptr(&(A),&(B))
riak_error
riak_binary_to_pb_deep_copy(riak_context        *ctx,
                            ProtobufCBinaryData *to,
                            riak_binary         *from);
void
riak_binary_from_pb_copy_ptr(riak_binary        *to,
                            ProtobufCBinaryData *from);
#define riak_binary_from_pb_copy(A,B) riak_binary_from_pb_copy_ptr(&(A),&(B))
riak_error
riak_binary_from_pb_deep_copy_ptr(riak_context        *ctx,
                                  riak_binary         *to,
                                  ProtobufCBinaryData *from);
#define riak_binary_from_pb_deep_copy(A,B,C) riak_binary_from_pb_deep_copy_ptr((A),&(B),&(C))

#endif /* RIAK_BINARY_H_ */
