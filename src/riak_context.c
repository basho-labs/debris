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

riak_context *riak_context_new(riak_alloc_fn alloc, riak_realloc_fn realloc, riak_free_fn freeme) {
    riak_alloc_fn   alloc_fn   = malloc;
    riak_realloc_fn realloc_fn = realloc;
    riak_free_fn    free_fn    = free;

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
    ctx->malloc_fn  = alloc_fn;
    ctx->realloc_fn = realloc_fn;
    ctx->free_fn    = free_fn;
    ctx->bevent     = NULL;

    return ctx;
}

void riak_context_set_event(riak_context *ctx, riak_bufferevent *bev) {
    ctx->bevent = bev;
}
