/*********************************************************************
 *
 * riak_context.c: Management of the Riak connection context
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

#include "riak.h"

extern ProtobufCAllocator protobuf_c_default_allocator;

riak_context *riak_context_new(riak_event_base  *base,
                               riak_bufferevent *bev,
                               riak_alloc_fn     alloc,
                               riak_realloc_fn   realloc,
                               riak_free_fn      freeme,
                               riak_pb_alloc_fn  pb_alloc,
                               riak_pb_free_fn   pb_free) {
    riak_alloc_fn   alloc_fn   = malloc;
    riak_realloc_fn realloc_fn = realloc;
    riak_free_fn    free_fn    = free;


    if (base == NULL || bev == NULL) {
        // TODO: Log message that these must be supplied
        assert(base != NULL);
        assert(bev != NULL);
    }
    if (alloc != NULL) {
        alloc_fn = alloc;
    }
    if (realloc != NULL) {
        realloc_fn = realloc;
    }
    if (freeme != NULL) {
        free_fn = freeme;
    }
    riak_context* ctx = (riak_context*)(alloc_fn)(sizeof(riak_context));
    if (ctx == NULL) {
        return NULL;
    }
    ctx->malloc_fn    = alloc_fn;
    ctx->realloc_fn   = realloc_fn;
    ctx->free_fn      = free_fn;
    ctx->base         = base;
    ctx->bevent       = bev;
    ctx->pb_allocator = NULL;
    if (pb_alloc != NULL && pb_free != NULL) {
        ctx->pb_allocator = &protobuf_c_default_allocator;
        ctx->pb_allocator->alloc = pb_alloc;
        ctx->pb_allocator->tmp_alloc = pb_alloc;
        ctx->pb_allocator->free = pb_free;
    }

    return ctx;
}
