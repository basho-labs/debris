/*********************************************************************
 *
 * riak_pb_message.c: Riak C Client Protocol Buffer Message
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

#include "riak_pb_message.h"
#include "riak_utils.h"
#include "riak.pb-c.h"
#include "riak_kv.pb-c.h"

int pb_encode_get_request(riak_context *ctx,
                          riak_binary *bucket,
                          riak_binary *key,
                          riak_get_options* get_options,
                          riak_pb_request *pb_req) {
    void *msgbuf;
    riak_uint32_t msglen;

    RpbGetReq getmsg = RPB_GET_REQ__INIT;

    getmsg.bucket.len = bucket->len;
    getmsg.bucket.data = bucket->data;

    getmsg.key.len = key->len;
    getmsg.key.data = key->data;

    // process get options
    if(get_options != NULL) {
         getmsg.has_r = get_options->has_r;
         getmsg.r = get_options->r;
         getmsg.has_pr = get_options->has_pr;
         getmsg.pr = get_options->pr;
         getmsg.has_basic_quorum = get_options->has_basic_quorum;
         getmsg.basic_quorum = get_options->basic_quorum;
         getmsg.has_notfound_ok = get_options->has_notfound_ok;
         getmsg.notfound_ok = get_options->notfound_ok;
         if(get_options->has_if_modified) {
            getmsg.has_if_modified = get_options->has_if_modified;
            getmsg.if_modified.len = get_options->if_modified.len;
            memcpy(&getmsg.if_modified.data, get_options->if_modified.data, get_options->if_modified.len);
         }
         getmsg.has_head = get_options->has_head;
         getmsg.head = get_options->head;
         getmsg.has_deletedvclock = get_options->has_deletedvclock;
         getmsg.deletedvclock = get_options->deletedvclock;
         getmsg.has_timeout = get_options->has_timeout;
         getmsg.timeout = get_options->timeout;
         getmsg.has_sloppy_quorum = get_options->has_sloppy_quorum;
         getmsg.sloppy_quorum = get_options->sloppy_quorum;
         getmsg.has_n_val = get_options->has_n_val;
         getmsg.n_val = get_options->n_val;
    }
    msglen = rpb_get_req__get_packed_size (&getmsg);
    msgbuf = malloc (msglen);
    rpb_get_req__pack (&getmsg, msgbuf);

    pb_req->reqid = MSG_RPBGETREQ;
    pb_req->msglength = msglen;
    pb_req->reqdata = msgbuf;
    return 0;
}


int pb_decode_get_response(riak_context *ctx,
                           riak_pb_response *pbresp,
                           riak_response *response) {
    // decode the PB response etc
    RpbGetResp *getresp = rpb_get_resp__unpack(NULL, pbresp->msglength, pbresp->respdata);
    int i = 0;
    if(getresp->n_content > 0) {
        response->objects = (riak_object*)malloc(sizeof(riak_object) * getresp->n_content);
        response->object_count = getresp->n_content;
        for(i = 0; i < getresp->n_content; i++) {
            RpbContent *c = getresp->content[i];
            riak_binary_populate(&(response->objects[i].value), c->value.len, c->value.data);
        }
    }
    rpb_get_resp__free_unpacked(getresp, NULL);
    // TODO: something will have to free pb_resp and the buffer inside of it
    return 0;
}



