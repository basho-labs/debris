/*********************************************************************
 *
 * riak_event.c: Management of the Riak event
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
#include <unistd.h>
#include "riak.h"
#include "riak_pb_message.h"
#include "riak_utils.h"
#include "riak_event.h"
#include "riak_network.h"

riak_event*
riak_event_new(riak_context          *ctx,
               riak_response_decoder  decoder,
               riak_response_callback response_cb,
               riak_response_callback error_cb,
               void                  *cb_data) {


    riak_event *rev = (riak_event*)(ctx->malloc_fn)(sizeof(riak_event));
    if (rev == NULL) {
        riak_log_context(ctx, RIAK_LOG_FATAL, "Could not allocate a riak_event");
        return NULL;
    }
    rev->base = riak_context_get_base(ctx); // Keep a copy until interface has settled
    rev->context = ctx;
    rev->decoder = decoder;
    rev->response_cb = response_cb;
    rev->error_cb = error_cb;
    rev->cb_data = cb_data;
    rev->position = 0;
    rev->msglen = 0;
    rev->msgbuf = NULL;
    rev->msglen_complete = RIAK_FALSE;

    // TODO: Implement retry logic
    rev->fd = riak_just_open_a_socket(ctx, ctx->addrinfo);
    if (rev->fd < 0) {
        riak_log_context(ctx, RIAK_LOG_FATAL, "Could not just open a socket");
        return NULL;
    }

    // rev->bevent = bufferevent_socket_new(base, sock, BEV_OPT_CLOSE_ON_FREE|BEV_OPT_DEFER_CALLBACKS|BEV_OPT_THREADSAFE);
    rev->bevent = bufferevent_socket_new(rev->base, rev->fd, BEV_OPT_CLOSE_ON_FREE|BEV_OPT_DEFER_CALLBACKS);
    if (rev->bevent == NULL) {
        riak_log_context(ctx, RIAK_LOG_FATAL, "Could not create bufferevent [fd %d]", rev->fd);
        return NULL;
    }
    int enabled = bufferevent_enable(rev->bevent, EV_READ|EV_WRITE);
    if (enabled != 0) {
        riak_log_context(ctx, RIAK_LOG_FATAL, "Could not enable bufferevent [fd %d]", rev->fd);
        return NULL;
    }

    // Set the internal read and write callbacks
    bufferevent_setcb(rev->bevent, riak_read_result_callback, riak_write_callback, riak_event_callback, rev);

    return rev;
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
riak_event_set_response_decoder(riak_event           *rev,
                                riak_response_decoder decoder) {
    rev->decoder = decoder;
}

void riak_event_free(riak_event** re) {
    riak_free_fn freer = (*re)->context->free_fn;
    if ((*re)->fd) close((*re)->fd);
    (freer)(*re);
    *re = NULL;
}
