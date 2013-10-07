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

#include <log4c.h>
#include "riak.h"
#include "riak_pb_message.h"
#include "riak_utils.h"
#include "riak_network.h"

extern ProtobufCAllocator protobuf_c_default_allocator;

riak_error
riak_context_new(riak_context    **context,
                 const char       *hostname,
                 const char       *portnum,
                 riak_alloc_fn     alloc,
                 riak_realloc_fn   realloc,
                 riak_free_fn      freeme,
                 riak_pb_alloc_fn  pb_alloc,
                 riak_pb_free_fn   pb_free,
                 const char       *logging_category) {
    *context                   = NULL;
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
        return ERIAK_OUT_OF_MEMORY;
    }
    ctx->malloc_fn    = alloc_fn;
    ctx->realloc_fn   = realloc_fn;
    ctx->free_fn      = free_fn;
    ctx->pb_allocator = NULL;
    if (pb_alloc != NULL && pb_free != NULL) {
        ctx->pb_allocator = &protobuf_c_default_allocator;
        ctx->pb_allocator->alloc = pb_alloc;
        ctx->pb_allocator->tmp_alloc = pb_alloc;
        ctx->pb_allocator->free = pb_free;
    }

    if (logging_category == NULL) {
        riak_strlcpy(ctx->logging_category, RIAK_LOGGING_DEFAULT_CATEGORY, sizeof(ctx->logging_category));
    } else {
        riak_strlcpy(ctx->logging_category, logging_category, sizeof(ctx->logging_category));
    }
    riak_strlcpy(ctx->hostname, hostname, sizeof(ctx->hostname));
    riak_strlcpy(ctx->portnum, portnum, sizeof(ctx->hostname));

    // Since we will likely only have one context, set up non-thread-safe logging here
    // TODO: Make logging thread-safe
    int result = log4c_init();
    if (result != 0) {
        fprintf(stderr, "Could not initialize logging\n");
        riak_context_free(&ctx);
        return ERIAK_LOGGING;
    }
    ctx->base = event_base_new();
    if (ctx->base == NULL) {
        riak_log_context(ctx, RIAK_LOG_FATAL, "Could not construct an event base");
        riak_context_free(&ctx);
        return ERIAK_EVENT;
    }

    riak_error err = riak_resolve_address(ctx, hostname, portnum, &(ctx->addrinfo));
    if (err) {
        riak_context_free(&ctx);
        return ERIAK_DNS_RESOLUTION;
    }
    *context = ctx;
    return ERIAK_OK;
}

riak_error
riak_context_new_default(riak_context** context,
                         char* hostname,
                         char* portnum) {
  return riak_context_new(context, hostname, portnum,
                          NULL,NULL,NULL,NULL,NULL,NULL);
}

riak_event_base*
riak_context_get_base(riak_context *ctx) {
    return ctx->base;
}

void
riak_context_free(riak_context **context) {
    riak_context *ctx = *context;
    riak_free_fn freer = ctx->free_fn;
    if (ctx->base == NULL) event_base_free(ctx->base);
    if (ctx->addrinfo) evutil_freeaddrinfo(ctx->addrinfo);
    (freer)(ctx);
    *context = NULL;

    // Since we will only clean up one context, let's shut down non-threadsafe logging here, too
    log4c_fini();
}
