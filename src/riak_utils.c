/*********************************************************************
 *
 * utils.c: Riak C Client Utilities
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
#include "riak_pb_message.h"
#include "riak_utils.h"

size_t
riak_strlcpy(char       *dst,
             const char *src,
             size_t      size) {
    if (size <= 0) return 0;

    size_t len;
    for(len = 0; (len < size) && (*src != '\0'); len++) {
        *dst++ = *src++;
    }
    *dst = '\0';

    return len;
}

size_t
riak_strlcat(char       *dst,
             const char *src,
             size_t      size) {
    if (size <= 0) return 0;

    size_t len;
    // Walk to the end of the first string
    for(len = 0; (len < size) && (*dst != '\0'); len++, dst++) {}
    for(; (len < size) && (*src != '\0'); len++) {
        *dst++ = *src++;
    }
    *dst = '\0';

    return len;
}

void riak_free_internal(riak_context *ctx, void **pp) {
    if(pp != NULL && *pp != NULL) {
        (ctx->free_fn)(*pp);
        *pp = NULL;
    }
}

riak_get_response*
riak_get_response_new(riak_context *ctx) {
    riak_get_response* r = (riak_get_response*)(ctx->malloc_fn)(sizeof(riak_get_response));
    bzero(r, sizeof(riak_get_response));
    return r;
}

void
riak_get_response_free(riak_context      *ctx,
                       riak_get_response *r) {
    if(r == 0) {
        return;
    }
    if(r->n_content > 0) {
        //for(i = 0; i < r->object_count; i++) {
        //  free_riak_object(&r->objects[i]);
        //}
    }
    riak_free(ctx, r);
}


riak_error
riak_send_req(riak_event      *rev,
              riak_pb_message *req) {
    riak_bufferevent *bev    = rev->bevent;
    riak_uint8_t      reqid  = req->msgid;
    riak_uint8_t     *msgbuf = req->data;
    riak_size_t       len    = req->len;
    // Convert len to network byte order
    ev_uint32_t msglen = htonl(len+1);
    int result = bufferevent_write(bev, (void*)&msglen, sizeof(msglen));
    if (result != 0) return ERIAK_WRITE;
    result = bufferevent_write(bev, (void*)&reqid, sizeof(reqid));
    if (result != 0) return ERIAK_WRITE;
    if (msglen > 0) {
        result = bufferevent_write(bev, (void*)msgbuf, len);
        if (result != 0) return ERIAK_WRITE;
    }
    riak_log(rev, RIAK_LOG_DEBUG, "Wrote %d bytes\n", (int)len);
    int i;
    for(i = 0; i < len; i++) {
        fprintf(stdout, "%02x", msgbuf[i]);
    }
    fprintf(stdout, "\n");
    for(i = 0; i < len; i++) {
        char c = '.';
        if (msgbuf[i] > 31 && msgbuf[i] < 128) {
            c = msgbuf[i];
        }
        fprintf(stdout, "%c", c);
    }

    fprintf(stdout, "\n");
    return ERIAK_OK;
}

