/*********************************************************************
 *
 * riak_types.h: Riak Opertions
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
#include "riak_kv.pb-c.h"
#include "riak_utils.h"

int riak_get(riak_context *ctx,
             riak_binary *bucket,
             riak_binary *key,
             riak_get_options *get_options,
             riak_response_callback resonse_cb) {
  return 0;
}

void write_callback(riak_bufferevent *bev, void *ptr)
{
    fprintf(stderr, "Ready for write with %0llx.\n", (riak_uint64_t)ptr);
}



void riak_list_buckets_callback(riak_bufferevent *bev, void *ptr) {
    riak_context *ctx = (riak_context*)ptr;
    uint32_t inmsglen;
    size_t buflen = bufferevent_read(bev, (void*)&inmsglen, sizeof(inmsglen));
    assert(buflen == 4);
    ev_uint8_t msglen = ntohl(inmsglen);
    ev_uint8_t buffer[8192];
    buflen = bufferevent_read(bev, (void*)buffer, 8192);
    assert(buflen > 0);
    ev_uint8_t msgtype = *buffer;
    fprintf(stderr, "Read msg [%d] %d of %d bytes\n", (unsigned char)msgtype, (int)msglen, (int)buflen);
    // Pull off packing

    if (msgtype == MSG_RPBERRORRESP) {
        RpbErrorResp *errresp = rpb_error_resp__unpack(NULL, buflen-1, (uint8_t*)(buffer+1));
        fprintf(stderr, "Err Unpacked\n");
        riak_uint32_t errcode = errresp->errcode;
        ProtobufCBinaryData binary = errresp->errmsg;
        char errmsg[8192];
        strncpy(errmsg, (char*)binary.data, 8192);
        errmsg[binary.len] = '\0';
        fprintf(stderr, "ERR #%d - %s\n", errcode, errmsg);
        exit(1);
    }
    RpbListBucketsResp *listbucketresp = rpb_list_buckets_resp__unpack(NULL /*allocator*/, buflen-1, (uint8_t*)(buffer+1));
    fprintf(stderr, "Unpacked\n");
    int i;
    char name[1024];
    for(i = 0; i < listbucketresp->n_buckets; i++) {
        ProtobufCBinaryData binary = listbucketresp->buckets[i];
        strncpy(name, (const char*)binary.data, binary.len);
        name[binary.len] = '\0';
        printf("%d - %s\n", i, name);
    }
    rpb_list_buckets_resp__free_unpacked(listbucketresp, NULL /*allocator*/);
    fprintf(stderr,"Freed\n");
}
