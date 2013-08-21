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

riak_pb_response *riak_pb_response_new(riak_context *ctx,
                                       riak_uint8_t  msgtype,
                                       riak_size_t   msglen,
                                       riak_uint8_t *buffer) {
    riak_pb_response *pb = (riak_pb_response*)(ctx->malloc_fn)(sizeof(riak_pb_response));
    if (pb != NULL) {
        pb->msgid = msgtype;
        pb->len   = msglen;
        pb->data  = buffer;
    }
    return pb;
}

void riak_pb_response_free(riak_context     *ctx,
                           riak_pb_response **pb) {
    riak_free(ctx, (*pb)->data);
    riak_free(ctx, (*pb));
}

int riak_decode_error_response(riak_event *ev, riak_pb_response *pbresp) {
    riak_context *ctx = (riak_context*)(ev->context);
    RpbErrorResp *errresp= rpb_error_resp__unpack(ctx->pb_allocator, (pbresp->len)-1, (uint8_t*)((pbresp->data)+1));
    uint32_t errcode = errresp->errcode;
    ProtobufCBinaryData binary = errresp->errmsg;
    char errmsg[8192];
    fprintf(stderr, "len = %d\n", (int)binary.len);
    strncpy(errmsg, (char*)binary.data, binary.len);
    errmsg[binary.len] = '\0';
    fprintf(stderr, "ERR #%d - %s\n", errcode, errmsg);
    rpb_error_resp__free_unpacked(errresp, ctx->pb_allocator);
    //TODO: Figure out how to register an error callback
    //riak_listbuckets_response_callback cb = (riak_error_response_callback)(ev->response_cb);
    //(cb)(response, ev->cb_data);
    exit(1);
}

int riak_encode_ping_request(riak_event *ev) {
    int result = riak_send_req(ev, MSG_RPBPINGREQ, NULL, 0);
    return result;
}

int riak_decode_ping_response(riak_event        *ev,
                              riak_pb_response  *pbresp) {
    riak_context *ctx = (riak_context*)(ev->context);
    riak_ping_response *response = (riak_ping_response*)(ctx->malloc_fn)(sizeof(riak_ping_response));
    if (response == NULL) {
        return 1;
    }
    riak_ping_response_callback cb = (riak_ping_response_callback)(ev->response_cb);
    (cb)(response, ev->cb_data);
    return 0;
}

int riak_encode_get_request(riak_event  *ev,
                            char *bucket,
                            char *key,
                            riak_get_options *get_options) {

    riak_context *ctx = (riak_context*)(ev->context);
    RpbGetReq getmsg = RPB_GET_REQ__INIT;

    getmsg.bucket.len = strlen(bucket);
    getmsg.bucket.data = (riak_uint8_t*)bucket;

    getmsg.key.len = strlen(key);
    getmsg.key.data = (riak_uint8_t*)key;

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
         if (get_options->has_if_modified) {
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
    riak_uint32_t msglen = rpb_get_req__get_packed_size (&getmsg);
    riak_uint8_t* msgbuf = (riak_uint8_t*)(ctx->malloc_fn)(msglen);
    if (msgbuf == NULL) {
        return 1;
    }
    rpb_get_req__pack (&getmsg, msgbuf);

    int result = riak_send_req(ev, MSG_RPBGETREQ, msgbuf, msglen);
    riak_free(ev->context, msgbuf);
    return result;
}

int riak_decode_get_response(riak_event        *ev,
                             riak_pb_response  *pbresp) {
    // decode the PB response etc
    riak_context *ctx = (riak_context*)(ev->context);
    RpbGetResp *rpbresp = rpb_get_resp__unpack(ctx->pb_allocator, (pbresp->len)-1, (uint8_t*)((pbresp->data)+1));
    fprintf(stderr, "riak_decode_get_response len=%d/pb unpack = 0x%lx\n", pbresp->len, (long)(rpbresp));
    if (rpbresp == NULL) {
        return 1;
    }
    int i = 0;
    riak_get_response *response = (riak_get_response*)(ctx->malloc_fn)(sizeof(riak_get_response));
    if (response == NULL) {
        return 1;
    }
    memset(response, '\0', sizeof(riak_get_response));

    if (rpbresp->has_vclock) {
        response->has_vclock = RIAK_TRUE;
        riak_binary_from_pb_deep_copy(ctx, response->vclock, rpbresp->vclock);
    }
    if (rpbresp->has_unchanged) {
        response->has_unmodified = RIAK_TRUE;
        response->unmodified = rpbresp->unchanged;
    }
    if (rpbresp->n_content > 0) {
        response->content = (riak_object*)(ctx->malloc_fn)(sizeof(riak_object) * rpbresp->n_content);
        if (response->content == NULL) {
            riak_free(ctx, response);
            return 1;
        }
        response->n_content = rpbresp->n_content;
        for(i = 0; i < rpbresp->n_content; i++) {
            riak_object_from_pb_copy(ctx, &(response->content[i]), rpbresp->content[i]);
        }
    }
    rpb_get_resp__free_unpacked(rpbresp, ctx->pb_allocator);
    riak_get_response_callback cb = (riak_get_response_callback)(ev->response_cb);
    (cb)(response, ev->cb_data);
    return 0;
}


int riak_encode_put_request(riak_event *ev,
                            riak_object *riak_obj,
                            riak_put_options *options) {

    riak_context *ctx = (riak_context*)(ev->context);
    RpbPutReq putmsg = RPB_PUT_REQ__INIT;

    riak_binary_to_pb_copy(putmsg.bucket, riak_obj->bucket);

    // Is the Key provided?
    if (riak_obj->has_key) {
        putmsg.has_key = RIAK_TRUE;
        riak_binary_to_pb_copy(putmsg.key, riak_obj->key);
    }

    // Data content payload
    RpbContent content;
    riak_object_to_pb_copy(ctx, &content, riak_obj);
    putmsg.content = &content;

    // process put options
    if (options != NULL) {
        if (options->has_asis) {
            putmsg.has_asis = RIAK_TRUE;
            putmsg.asis = options->asis;
        }
        if (options->has_dw) {
            putmsg.has_dw = RIAK_TRUE;
            putmsg.dw = options->dw;
        }
        if (options->has_if_none_match) {
            putmsg.has_if_none_match = RIAK_TRUE;
            putmsg.has_if_none_match = options->has_if_none_match;
        }
        if (options->has_if_not_modified) {
            putmsg.has_if_not_modified = RIAK_TRUE;
            putmsg.has_if_not_modified = options->has_if_not_modified;
        }
        if (options->has_n_val) {
            putmsg.has_n_val = RIAK_TRUE;
            putmsg.n_val = options->n_val;
        }
        if (options->has_pw) {
            putmsg.has_pw = RIAK_TRUE;
            putmsg.pw = options->pw;
        }
        if (options->has_return_body) {
            putmsg.has_return_body = RIAK_TRUE;
            putmsg.return_body = options->return_body;
        }
        if (options->has_return_head) {
            putmsg.has_return_head = RIAK_TRUE;
            putmsg.return_head = options->return_head;
        }
        if (options->has_sloppy_quorum) {
            putmsg.has_sloppy_quorum = RIAK_TRUE;
            putmsg.sloppy_quorum = options->sloppy_quorum;
        }
        if (options->has_timeout) {
            putmsg.has_timeout = RIAK_TRUE;
            putmsg.timeout = options->timeout;
        }
        if (options->has_vclock) {
            putmsg.has_vclock = RIAK_TRUE;
            riak_binary_to_pb_copy(putmsg.vclock, options->vclock);
        }
        if (options->has_w) {
            putmsg.has_w = RIAK_TRUE;
            putmsg.w = options->w;
        }
    }

    riak_uint32_t msglen = rpb_put_req__get_packed_size (&putmsg);
    riak_uint8_t* msgbuf = (riak_uint8_t*)(ctx->malloc_fn)(msglen);
    if (msgbuf == NULL) {
        return 1;
    }
    rpb_put_req__pack (&putmsg, msgbuf);

    riak_object_free_pb(ctx, &content);

    int result = riak_send_req(ev, MSG_RPBPUTREQ, msgbuf, msglen);
    riak_free(ev->context, msgbuf);
    return result;
}

int riak_decode_put_response(riak_event        *ev,
                             riak_pb_response  *pbresp) {
    // decode the PB response etc
    riak_context *ctx = (riak_context*)(ev->context);
    RpbPutResp *rpbresp = rpb_put_resp__unpack(ctx->pb_allocator, (pbresp->len)-1, (uint8_t*)((pbresp->data)+1));
    fprintf(stderr, "riak_decode_put_response len=%d/pb unpack = 0x%lx\n", pbresp->len, (long)(rpbresp));
    if (rpbresp == NULL) {
        return 1;
    }
    int i = 0;
    riak_put_response *response = (riak_put_response*)(ctx->malloc_fn)(sizeof(riak_put_response));
    if (response == NULL) {
        return 1;
    }
    memset(response, '\0', sizeof(riak_put_response));
    if (rpbresp->has_vclock) {
        response->has_vclock = RIAK_TRUE;
        riak_binary_from_pb_copy(response->vclock, rpbresp->vclock);
    }
    if (rpbresp->has_key) {
        riak_binary_from_pb_copy(response->key, rpbresp->key);
    }
    if (rpbresp->n_content > 0) {
        response->content = (riak_object*)(ctx->malloc_fn)(sizeof(riak_object) * rpbresp->n_content);
        response->n_content = rpbresp->n_content;
        for(i = 0; i < rpbresp->n_content; i++) {
            riak_object_from_pb_copy(ctx, &(response->content[i]), rpbresp->content[i]);
        }
    }
    rpb_put_resp__free_unpacked(rpbresp, ctx->pb_allocator);
    riak_put_response_callback cb = (riak_put_response_callback)(ev->response_cb);
    (cb)(response, ev->cb_data);
    return 0;
}

int riak_encode_delete_request(riak_event          *ev,
                               riak_binary         *bucket,
                               riak_binary         *key,
                               riak_delete_options *options) {

    riak_context *ctx = (riak_context*)(ev->context);
    RpbDelReq delmsg = RPB_DEL_REQ__INIT;

    riak_binary_to_pb_copy_ptr(&delmsg.bucket, bucket);
    riak_binary_to_pb_copy_ptr(&delmsg.key, key);

    // process delete options
    if (options != NULL) {
        if (options->has_dw) {
            delmsg.has_dw = RIAK_TRUE;
            delmsg.dw = options->dw;
        }
        if (options->has_n_val) {
            delmsg.has_n_val = RIAK_TRUE;
            delmsg.n_val = options->n_val;
        }
        if (options->has_pw) {
            delmsg.has_pw = RIAK_TRUE;
            delmsg.pw = options->pw;
        }

        if (options->has_sloppy_quorum) {
            delmsg.has_sloppy_quorum = RIAK_TRUE;
            delmsg.sloppy_quorum = options->sloppy_quorum;
        }
        if (options->has_timeout) {
            delmsg.has_timeout = RIAK_TRUE;
            delmsg.timeout = options->timeout;
        }
        if (options->has_vclock) {
            delmsg.has_vclock = RIAK_TRUE;
            riak_binary_to_pb_copy(delmsg.vclock, options->vclock);
        }
        if (options->has_w) {
            delmsg.has_w = RIAK_TRUE;
            delmsg.w = options->w;
        }
    }

    riak_uint32_t msglen = rpb_del_req__get_packed_size (&delmsg);
    riak_uint8_t* msgbuf = (riak_uint8_t*)(ctx->malloc_fn)(msglen);
    if (msgbuf == NULL) {
        return 1;
    }
    rpb_del_req__pack (&delmsg, msgbuf);

    int result = riak_send_req(ev, MSG_RPBDELREQ, msgbuf, msglen);
    riak_free(ev->context, msgbuf);
    return result;
}

int riak_decode_delete_response(riak_event        *ev,
                                riak_pb_response  *pbresp) {
    riak_context *ctx = (riak_context*)(ev->context);
    riak_delete_response *response = (riak_delete_response*)(ctx->malloc_fn)(sizeof(riak_delete_response));
    if (response == NULL) {
        return 1;
    }
    riak_delete_response_callback cb = (riak_delete_response_callback)(ev->response_cb);
    (cb)(response, ev->cb_data);
    return 0;
}

// TODO: Should encode be separate from sending?
int riak_encode_listbuckets_request(riak_event *ev) {
    riak_context *ctx = (riak_context*)(ev->context);
    RpbListBucketsReq listbucketsreq = RPB_LIST_BUCKETS_REQ__INIT;
    listbucketsreq.stream = RIAK_FALSE;
    listbucketsreq.has_stream = RIAK_TRUE;
    riak_size_t msglen = rpb_list_buckets_req__get_packed_size(&listbucketsreq);
    riak_uint8_t *msgbuf = (riak_uint8_t*)(ctx->malloc_fn)(msglen);
    if (msgbuf == NULL) {
        return 1;
    }
    rpb_list_buckets_req__pack(&listbucketsreq, msgbuf);

    int result = riak_send_req(ev, MSG_RPBLISTBUCKETSREQ, msgbuf, msglen);
    riak_free(ev->context, msgbuf);
    return result;
}

int riak_decode_listbuckets_response(riak_event *ev, riak_pb_response *pbresp) {
    fprintf(stderr, "riak_decode_listbuckets_response\n");
    riak_context *ctx = (riak_context*)(ev->context);
    RpbListBucketsResp *listbucketresp = rpb_list_buckets_resp__unpack(ctx->pb_allocator, (pbresp->len)-1, (uint8_t*)((pbresp->data)+1));
    int i;
    riak_listbuckets_response *response = (ctx->malloc_fn)(sizeof(riak_listbuckets_response));
    response->buckets = (riak_binary*)(ctx->malloc_fn)(sizeof(riak_binary)*listbucketresp->n_buckets);
    if (response->buckets == NULL) {
        return 1;
    }
    response->n_buckets = listbucketresp->n_buckets;
    for(i = 0; i < listbucketresp->n_buckets; i++) {
        ProtobufCBinaryData binary = listbucketresp->buckets[i];
        response->buckets[i].data = (riak_uint8_t*)(ctx->malloc_fn)(binary.len);
        if (response->buckets[i].data == NULL) {
            // TODO: Cleanup existing allocated memory
            return 1;
        }
        memcpy(response->buckets[i].data, binary.data, binary.len);
        response->buckets[i].len = binary.len;
    }
    response->done = RIAK_FALSE;
    if (listbucketresp->has_done) {
        fprintf(stderr, "HAS DONE\n");
        response->done = listbucketresp->done;
    }
    fprintf(stderr, "pb done = %d\n", listbucketresp->done);
    rpb_list_buckets_resp__free_unpacked(listbucketresp, ctx->pb_allocator);
    riak_listbuckets_response_callback cb = (riak_listbuckets_response_callback)(ev->response_cb);
    (cb)(response, ev->cb_data);
    return 0;
}


int riak_encode_listkeys_request(riak_event *ev,
                                 char *bucket,
                                 riak_uint32_t timeout) {
    riak_context *ctx = (riak_context*)(ev->context);
    RpbListKeysReq listkeysreq = RPB_LIST_KEYS_REQ__INIT;
    riak_binary binbucket;
    binbucket.data = (riak_uint8_t*)bucket;
    binbucket.len = strlen(bucket);
    riak_binary_to_pb_copy(listkeysreq.bucket, binbucket);
    if (timeout > 0) {
        listkeysreq.has_timeout = RIAK_TRUE;
        listkeysreq.timeout = timeout;
    }
    riak_size_t msglen = rpb_list_keys_req__get_packed_size(&listkeysreq);
    riak_uint8_t *msgbuf = (riak_uint8_t*)(ctx->malloc_fn)(msglen);
    if (msgbuf == NULL) {
        return 1;
    }
    rpb_list_keys_req__pack(&listkeysreq, msgbuf);

    int result = riak_send_req(ev, MSG_RPBLISTKEYSREQ, msgbuf, msglen);
    riak_free(ev->context, msgbuf);
    return result;
}

int riak_decode_listkeys_response(riak_event       *ev,
                                  riak_pb_response *pbresp) {
    fprintf(stderr, "riak_decode_listkeys_response\n");
    riak_context *ctx = (riak_context*)(ev->context);
    RpbListKeysResp *listkeyresp = rpb_list_keys_resp__unpack(ctx->pb_allocator, (pbresp->len)-1, (uint8_t*)((pbresp->data)+1));
    int i;
    riak_listkeys_response *response = (ctx->malloc_fn)(sizeof(riak_listkeys_response));
    response->keys = (riak_binary*)(ctx->malloc_fn)(sizeof(riak_binary)*listkeyresp->n_keys);
    if (response->keys == NULL) {
        return 1;
    }
    response->n_keys = listkeyresp->n_keys;
    for(i = 0; i < listkeyresp->n_keys; i++) {
        ProtobufCBinaryData binary = listkeyresp->keys[i];
        response->keys[i].data = (riak_uint8_t*)(ctx->malloc_fn)(binary.len);
        if (response->keys[i].data == NULL) {
            // TODO: Cleanup existing allocated memory
            return 1;
        }
        memcpy(response->keys[i].data, binary.data, binary.len);
        response->keys[i].len = binary.len;
    }
    response->done = RIAK_FALSE;
    if (listkeyresp->has_done) {
        fprintf(stderr, "HAS DONE\n");
        response->done = listkeyresp->done;
    }
    fprintf(stderr, "pb done = %d\n", listkeyresp->done);
    rpb_list_keys_resp__free_unpacked(listkeyresp, ctx->pb_allocator);
    riak_listkeys_response_callback cb = (riak_listkeys_response_callback)(ev->response_cb);
    (cb)(response, ev->cb_data);
    return 0;
}

// MAIN RESULT CALLBACK
void riak_read_result_callback(riak_bufferevent *bev, void *ptr) {
    riak_event   *ev = (riak_event*)ptr;
    riak_context *ctx = (riak_context*)(ev->context);
    // Read the first 32-bits which are the message size
    riak_uint32_t inmsglen;
    riak_size_t buflen = bufferevent_read(bev, (void*)&inmsglen, sizeof(inmsglen));
    // TODO: Real error checking
    assert(buflen == 4);
    riak_uint32_t msglen = ntohl(inmsglen);
    riak_uint8_t *buffer = (riak_uint8_t*)(ctx->malloc_fn)(msglen);
    // TODO: Real error checking
    assert(buffer != 0);
    buflen = bufferevent_read(bev, (void*)buffer, msglen);
    assert(buflen == msglen);
    // TODO: Anything else on the wire?
    riak_uint8_t msgid = buffer[0];
    riak_pb_response *pbresp = riak_pb_response_new(ctx, msgid, msglen, buffer);
    int result;

   switch (msgid) {
    case MSG_RPBERRORRESP:
        result = riak_decode_error_response(ev, pbresp);
        break;
    case MSG_RPBPINGRESP:
        result = riak_decode_ping_response(ev, pbresp);
        break;
    case MSG_RPBGETRESP:
         result = riak_decode_get_response(ev, pbresp);
        break;
    case MSG_RPBPUTRESP:
        result = riak_decode_put_response(ev, pbresp);
        break;
    case MSG_RPBDELRESP:
        result = riak_decode_delete_response(ev, pbresp);
        break;
    case MSG_RPBLISTBUCKETSRESP:
        result = riak_decode_listbuckets_response(ev, pbresp);
        break;
    case MSG_RPBLISTKEYSRESP:
        result = riak_decode_listkeys_response(ev, pbresp);
        break;
    case MSG_RPBGETCLIENTIDRESP:
    case MSG_RPBSETCLIENTIDRESP:
    case MSG_RPBGETSERVERINFORESP:
    case MSG_RPBGETBUCKETRESP:
    case MSG_RPBSETBUCKETRESP:
    case MSG_RPBMAPREDRESP:
    case MSG_RPBINDEXRESP:
    case MSG_RBPSEARCHQUERYRESP:
        fprintf(stderr, "%d NOT IMPLEMENTED\n", msgid);
        abort();
    }
    riak_pb_response_free(ctx, &pbresp);
    bufferevent_free(bev);

    // What has been queued up
    event_base_dump_events(ev->base, stderr);

    // TODO: Should user control the event loop?  Should we include bev in response?
    //event_base_loopexit(ev->base, NULL);
}
