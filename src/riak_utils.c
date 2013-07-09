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
#include "riak_utils.h"
#include "riak_pb_message.h"

void _riak_free(riak_context *ctx, void **pp) {
    if(pp != NULL && *pp != NULL) {
        (ctx->free_fn)(*pp);
        *pp = NULL;
    }
}

// RIAK_OBJECT
riak_object *riak_object_new(riak_context *ctx) {
    riak_object *o = (riak_object*)(ctx->malloc_fn)(sizeof(riak_object));
    // TODO: check malloc return status
    // TODO: do I need bzero?
    bzero(o, sizeof(riak_object));
    return o;
}

void riak_object_free(riak_context *ctx, riak_object *o) {
    if (o == NULL) {
        return;
    }
    riak_free(ctx, o);
}

// RIAK_BINARY
// be careful
riak_binary *riak_binary_new(riak_context *ctx, riak_size_t len, riak_uint8_t *data) {
      riak_binary *b = (riak_binary*)malloc(sizeof(riak_binary));
      b->len  = len;
      b->data = (riak_uint8_t*)(ctx->malloc_fn)(len);
      // TODO: Check malloc return status
      memcpy((void*)b->data, (void*)data, len);
      return b;
}

void riak_binary_populate(riak_context *ctx, riak_binary *b, riak_size_t len, riak_uint8_t *data) {
      b->len  = len;
      b->data = (uint8_t*)(ctx->malloc_fn)(len);
      // TODO: Check malloc return status
      memcpy((void*)b->data, (void*)data, len);
}

void riak_binary_free(riak_context *ctx, riak_binary *b) {
      if (b == NULL) {
          return;
      }
      riak_free(ctx, b->data);
      riak_free(ctx, b);
}

void riak_binary_copy(riak_binary* to, riak_binary* from) {
    to->len  = from->len;
    to->data = from->data;
}

void riak_binary_deep_copy(riak_context *ctx, riak_binary *to, riak_binary *from) {
    to->len  = from->len;
    to->data = (riak_uint8_t*)(ctx->malloc_fn)(from->len);
    // TODO: Check malloc return status
    memcpy((void*)to->data, (void*)from->data, from->len);
}


riak_get_response* riak_get_response_new(riak_context *ctx) {
    riak_get_response* r = (riak_get_response*)(ctx->malloc_fn)(sizeof(riak_get_response));
    bzero(r, sizeof(riak_get_response));
    return r;
}

void riak_get_response_free(riak_context *ctx, riak_get_response *r) {
    if(r == 0) {
        return;
    }
    if(r->object_count > 0) {
        //for(i = 0; i < r->object_count; i++) {
        //  free_riak_object(&r->objects[i]);
        //}
    }
    riak_free(ctx, r);
}

/* LIBEVENT CALLBACKS */

void eventcb(struct bufferevent *bev, short events, void *ptr)
{
    if (events & BEV_EVENT_CONNECTED) {
         fprintf(stderr, "Connect okay.\n");
    } else if (events & (BEV_EVENT_ERROR|BEV_EVENT_EOF)) {
         struct event_base *base = ptr;
         if (events & BEV_EVENT_ERROR) {
            int err = bufferevent_socket_get_dns_error(bev);
            if (err)
                 printf("DNS error: %s\n", evutil_gai_strerror(err));
         }
         fprintf(stderr, "Closing\n");
         bufferevent_free(bev);
         event_base_loopexit(base, NULL);
    } else {
        fprintf(stderr, "Event %d\n", events);
    }
}

int riak_send_req(riak_context *ctx, riak_uint8_t reqid, riak_uint8_t *msgbuf, riak_size_t len) {
    riak_bufferevent *bev = ctx->bevent;
    // Convert len to network byte order
    ev_uint32_t msglen = htonl(len+1);
    int result = bufferevent_write(bev, (void*)&msglen, sizeof(msglen));
    if (result != 0) return 1;
    result = bufferevent_write(bev, (void*)&reqid, sizeof(reqid));
    if (result != 0) return 1;
    if (msglen > 0) {
        result = bufferevent_write(bev, (void*)msgbuf, len);
        if (result != 0) return 1;
    }
    fprintf(stderr, "Wrote %d bytes\n", (int)len);
    return 0;
}

