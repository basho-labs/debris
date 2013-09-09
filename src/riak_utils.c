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

void _riak_free(riak_context *ctx, void **pp) {
    if(pp != NULL && *pp != NULL) {
        (ctx->free_fn)(*pp);
        *pp = NULL;
    }
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

void riak_binary_copy_ptr(riak_binary* to, riak_binary* from) {
    to->len  = from->len;
    to->data = from->data;
}

void
riak_binary_deep_copy(riak_context *ctx,
                      riak_binary *to,
				      riak_binary *from) {
    to->len  = from->len;
    to->data = (riak_uint8_t*)(ctx->malloc_fn)(from->len);
    // TODO: Check malloc return status
    memcpy((void*)to->data, (void*)from->data, from->len);
}

void
riak_binary_to_pb_copy_ptr(ProtobufCBinaryData* to,
                           riak_binary* from) {
    to->len  = from->len;
    to->data = from->data;
}

void
riak_binary_to_pb_deep_copy(riak_context        *ctx,
                            ProtobufCBinaryData *to,
                            riak_binary         *from) {
    to->len  = from->len;
    to->data = (riak_uint8_t*)(ctx->malloc_fn)(from->len);
    // TODO: Check malloc return status
    memcpy((void*)to->data, (void*)from->data, from->len);
}

void
riak_binary_from_pb_copy_ptr(riak_binary         *to,
                             ProtobufCBinaryData *from) {
    to->len  = from->len;
    to->data = from->data;
}

void
riak_binary_from_pb_deep_copy_ptr(riak_context        *ctx,
                                  riak_binary         *to,
                                  ProtobufCBinaryData *from) {
    to->len  = from->len;
    to->data = (riak_uint8_t*)(ctx->malloc_fn)(from->len);
    // TODO: Check malloc return status
    memcpy((void*)to->data, (void*)from->data, from->len);
}

//TODO: Figure out clean way to print UTF-8 encoding
int
riak_binary_dump_ptr(riak_binary *bin,
                     char* target,
                     riak_uint32_t len) {
    int count = 0;
    for( ; count < bin->len && count < len-1; count++) {
        char c = '.';
        // Non-printable characters are replaced by a dot
        if (bin->data[count] >= 32) c = bin->data[count];
        target[count] = c;
    }
    if (len > 0) target[count] = '\0';
    return count;
}

int
riak_binary_hex_dump_ptr(riak_binary *bin,
                         char* target,
                         riak_uint32_t len) {
    int count = 0;
    static char hex[] = "0123456789abcdef";
    if (bin != NULL) {
        for( ; count < bin->len && (count*2) < len-1; count++) {

            int nibble = (bin->data[count] & 0xf0) >> 4;
            target[count*2] = hex[nibble];
            nibble = (bin->data[count] & 0x0f);
            target[count*2+1] = hex[nibble];
        }
    }
    if (len > 0) target[count*2] = '\0';
    return count*2;
}

riak_get_response*
riak_get_response_new(riak_context *ctx) {
    riak_get_response* r = (riak_get_response*)(ctx->malloc_fn)(sizeof(riak_get_response));
    bzero(r, sizeof(riak_get_response));
    return r;
}

void
riak_get_response_free(riak_context *ctx,
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


int
riak_send_req(riak_event   *rev,
              riak_uint8_t  reqid,
              riak_uint8_t *msgbuf,
              riak_size_t   len) {
    riak_context     *ctx = rev->context;
    riak_bufferevent *bev = rev->bevent;
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
    riak_log(ctx, RIAK_LOG_DEBUG, "Wrote %d bytes\n", (int)len);
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
    return 0;
}

