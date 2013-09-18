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

extern ProtobufCAllocator protobuf_c_default_allocator;

riak_context *riak_context_new(riak_alloc_fn     alloc,
                               riak_realloc_fn   realloc,
                               riak_free_fn      freeme,
                               riak_pb_alloc_fn  pb_alloc,
                               riak_pb_free_fn   pb_free,
                               const char       *logging_category) {
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
        riak_strlcpy(ctx->logging_category, RIAK_LOGGING_DEFAULT_CATEGORY, RIAK_LOGGING_MAX_LEN);
    } else {
        riak_strlcpy(ctx->logging_category, logging_category, RIAK_LOGGING_MAX_LEN);
    }

    // Since we will likely only have one context, set up non-thread-safe logging here
    // TODO: Make logging thread-safe
    int result = log4c_init();
    if (result != 0) {
        fprintf(stderr, "Could not initialize logging\n");
        exit(1);
    }

    return ctx;
}

void riak_context_free(riak_context **ctx) {
    riak_free_fn freer = (*ctx)->free_fn;
    (freer)(*ctx);
    *ctx = NULL;

    // Since we will only clean up one context, let's shut down non-threadsafe logging here, too
    log4c_fini();
}

riak_event*
riak_event_new(riak_context          *ctx,
               riak_event_base       *base,
               riak_bufferevent      *bev,
               riak_response_decoder  decoder,
               riak_response_callback response_cb,
               riak_response_callback error_cb,
               void                  *cb_data) {
    if (base == NULL || bev == NULL) {
        riak_log(ctx, RIAK_LOG_FATAL, "Both riak_event_base and riak_bufferevent must be supplied to riak_event_new");
        assert(base != NULL);
        assert(bev != NULL);
    }
    riak_event *ev = (riak_event*)(ctx->malloc_fn)(sizeof(riak_event));
    // TODO: Error checking
    assert(ev != NULL);
    ev->base = base;
    ev->bevent = bev;
    ev->context = ctx;
    ev->decoder = decoder;
    ev->response_cb = response_cb;
    ev->error_cb = error_cb;
    ev->cb_data = cb_data;

    return ev;
}

void
riak_event_set_cb_data(riak_event *rev,
                       void       *cb_data) {
    rev->cb_data = cb_data;
}
void
riak_event_set_response_cb(riak_event             *rev,
                           riak_response_callback  cb) {
    rev->response_cb = cb;
}

void
riak_event_set_error_cb(riak_event             *rev,
                        riak_response_callback  cb) {
    rev->error_cb = cb;
}

void
riak_event_set_response_decoder(riak_event             *rev,
                                riak_response_decoder   decoder) {
    rev->decoder = decoder;
}

void riak_event_free(riak_event** re) {
    riak_free_fn freer = (*re)->context->free_fn;
    (freer)(*re);
    *re = NULL;
}
