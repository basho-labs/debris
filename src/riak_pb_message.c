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

#include <unistd.h>
#include "riak_pb_message.h"
#include "riak_utils.h"
#include "riak_binary.h"
#include "riak.pb-c.h"
#include "riak_kv.pb-c.h"

riak_pb_message*
riak_pb_message_new(riak_context *ctx,
                    riak_uint8_t  msgtype,
                    riak_size_t   msglen,
                    riak_uint8_t *buffer) {
    riak_pb_message *pb = (riak_pb_message*)(ctx->malloc_fn)(sizeof(riak_pb_message));
    if (pb != NULL) {
        pb->msgid   = msgtype;
        pb->len     = msglen;
        pb->data    = buffer;
    }
    return pb;
}

void
riak_pb_message_free(riak_context      *ctx,
                     riak_pb_message **pb) {
    riak_free(ctx, (*pb)->data);
    riak_free(ctx, (*pb));
}

riak_error
riak_decode_error_response(riak_event           *rev,
                           riak_pb_message      *pbresp,
                           riak_error_response **resp,
                           riak_boolean_t       *done) {
    riak_context *ctx = (riak_context*)(rev->context);
    RpbErrorResp *errresp = rpb_error_resp__unpack(ctx->pb_allocator, (pbresp->len)-1, (uint8_t*)((pbresp->data)+1));
    riak_error_response *response = (riak_error_response*)(ctx->malloc_fn)(sizeof(riak_error_response));
    *done = RIAK_TRUE;
    if (response == NULL) {
        rpb_error_resp__free_unpacked(errresp, ctx->pb_allocator);
        return ERIAK_OUT_OF_MEMORY;
    }
    response->_internal = errresp;
    response->errcode = errresp->errcode;
    riak_binary_from_pb_copy(response->errmsg, errresp->errmsg);
    // Call user's error callback, if present
    if (rev->error_cb) {
        riak_response_callback cb = rev->error_cb;
        (cb)(response, rev->cb_data);
    }
    *resp = response;

    return ERIAK_OK;
}

void
riak_free_error_response(riak_event           *rev,
                         riak_error_response **resp) {
    riak_context *ctx = (riak_context*)(rev->context);
    riak_error_response *response = *resp;
    rpb_error_resp__free_unpacked(response->_internal, ctx->pb_allocator);
    riak_free_ptr(ctx, resp);
}


riak_error
riak_decode_ping_response(riak_event          *rev,
                          riak_pb_message     *pbresp,
                          riak_ping_response **resp,
                          riak_boolean_t      *done) {
    riak_context *ctx = (riak_context*)(rev->context);
    riak_ping_response *response = (riak_ping_response*)(ctx->malloc_fn)(sizeof(riak_ping_response));
    *done = RIAK_TRUE;
    if (response == NULL) {
        return ERIAK_OUT_OF_MEMORY;
    }
    *resp = response;

    return ERIAK_OK;
}

riak_error
riak_encode_ping_request(riak_event      *rev,
                        riak_pb_message **req) {
    riak_context *ctx = (riak_context*)(rev->context);
    riak_pb_message* request = riak_pb_message_new(ctx, MSG_RPBPINGREQ, 0, NULL);
    if (request == NULL) {
        return ERIAK_OUT_OF_MEMORY;
    }
    *req = request;
    riak_event_set_response_decoder(rev, (riak_response_decoder)riak_decode_ping_response);

    return ERIAK_OK;
}


void
riak_free_ping_response(riak_event           *rev,
                         riak_ping_response **resp) {
    riak_context *ctx = (riak_context*)(rev->context);
    riak_free_ptr(ctx, resp);
}

riak_error
riak_encode_get_request(riak_event       *rev,
                        riak_binary      *bucket,
                        riak_binary      *key,
                        riak_get_options *get_options,
                        riak_pb_message **req) {

    riak_context *ctx = (riak_context*)(rev->context);
    RpbGetReq getmsg = RPB_GET_REQ__INIT;

    riak_binary_to_pb_copy_ptr(&(getmsg.bucket), bucket);
    riak_binary_to_pb_copy_ptr(&(getmsg.key), key);

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
        return ERIAK_OUT_OF_MEMORY;
    }
    rpb_get_req__pack (&getmsg, msgbuf);
    riak_pb_message* request = riak_pb_message_new(ctx, MSG_RPBGETREQ, msglen, msgbuf);
    if (request == NULL) {
        return ERIAK_OUT_OF_MEMORY;
    }
    *req = request;
    riak_event_set_response_decoder(rev, (riak_response_decoder)riak_decode_get_response);

    return ERIAK_OK;
}

riak_error
riak_decode_get_response(riak_event         *rev,
                         riak_pb_message    *pbresp,
                         riak_get_response **resp,
                         riak_boolean_t     *done) {
    // decode the PB response etc
    riak_context *ctx = (riak_context*)(rev->context);
    RpbGetResp *rpbresp = rpb_get_resp__unpack(ctx->pb_allocator, (pbresp->len)-1, (uint8_t*)((pbresp->data)+1));
    riak_log(rev, RIAK_LOG_DEBUG, "riak_decode_get_response len=%d/pb unpack = 0x%lx\n", pbresp->len, (long)(rpbresp));
    *done = RIAK_TRUE;
    if (rpbresp == NULL) {
        return ERIAK_OUT_OF_MEMORY;
    }
    int i = 0;
    riak_get_response *response = (riak_get_response*)(ctx->malloc_fn)(sizeof(riak_get_response));
    if (response == NULL) {
        return ERIAK_OUT_OF_MEMORY;
    }
    bzero(response, sizeof(riak_get_response));
    response->_internal = rpbresp;

    if (rpbresp->has_vclock) {
        response->has_vclock = RIAK_TRUE;
        riak_binary_from_pb_copy(response->vclock, rpbresp->vclock);
    }
    if (rpbresp->has_unchanged) {
        response->has_unmodified = RIAK_TRUE;
        response->unmodified = rpbresp->unchanged;
    }
    if (rpbresp->n_content > 0) {
        riak_error err = riak_object_new_array(ctx, &(response->content), rpbresp->n_content);
        if (err != ERIAK_OK) {
            riak_free(ctx, response);
            return 1;
        }
        response->n_content = rpbresp->n_content;
        for(i = 0; i < rpbresp->n_content; i++) {
            err = riak_object_new_from_pb(ctx, &(response->content[i]), rpbresp->content[i]);
            // If any object allocation fails, clean up all previously allocated ones
            if (err != ERIAK_OK) {
                riak_object_free_array(ctx, &(response->content), i);
                riak_free(ctx, response);
                return err;
            }
        }
    }
    *resp = response;

    return ERIAK_OK;
}

void
riak_free_get_response(riak_event         *rev,
                       riak_get_response **resp) {
    riak_context *ctx = (riak_context*)(rev->context);
    riak_get_response *response = *resp;
    if (response->n_content > 0) {
        riak_object_free_array(ctx, &(response->content), response->n_content);
    }
    rpb_get_resp__free_unpacked(response->_internal, ctx->pb_allocator);
    riak_free_ptr(ctx, resp);
}


riak_error
riak_encode_put_request(riak_event       *rev,
                        riak_object      *riak_obj,
                        riak_put_options *options,
                        riak_pb_message **req) {

    riak_context *ctx = (riak_context*)(rev->context);
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
        return ERIAK_OUT_OF_MEMORY;
    }
    rpb_put_req__pack (&putmsg, msgbuf);

    riak_pb_message* request = riak_pb_message_new(ctx, MSG_RPBPUTREQ, msglen, msgbuf);
    if (request == NULL) {
        return ERIAK_OUT_OF_MEMORY;
    }
    *req = request;
    riak_event_set_response_decoder(rev, (riak_response_decoder)riak_decode_put_response);

    return ERIAK_OK;
}

void
riak_free_put_request(riak_event         *rev,
                      riak_put_response **resp) {
    riak_context *ctx = (riak_context*)(rev->context);
    riak_put_response *response = *resp;
    rpb_put_resp__free_unpacked(response->_internal, ctx->pb_allocator);
    riak_free_ptr(ctx, resp);
}

riak_error
riak_decode_put_response(riak_event         *rev,
                         riak_pb_message    *pbresp,
                         riak_put_response **resp,
                         riak_boolean_t     *done) {
    // decode the PB response etc
    riak_context *ctx = (riak_context*)(rev->context);
    RpbPutResp *rpbresp = rpb_put_resp__unpack(ctx->pb_allocator, (pbresp->len)-1, (uint8_t*)((pbresp->data)+1));
    *done = RIAK_TRUE;
    riak_log(rev, RIAK_LOG_DEBUG, "riak_decode_put_response len=%d/pb unpack = 0x%lx\n", pbresp->len, (long)(rpbresp));
    if (rpbresp == NULL) {
        return ERIAK_OUT_OF_MEMORY;
    }
    int i = 0;
    riak_put_response *response = (riak_put_response*)(ctx->malloc_fn)(sizeof(riak_put_response));
    if (response == NULL) {
        return ERIAK_OUT_OF_MEMORY;
    }
    bzero(response, sizeof(riak_put_response));
    if (rpbresp->has_vclock) {
        response->has_vclock = RIAK_TRUE;
        riak_binary_from_pb_copy(response->vclock, rpbresp->vclock);
    }
    if (rpbresp->has_key) {
        riak_binary_from_pb_copy(response->key, rpbresp->key);
    }
    if (rpbresp->n_content > 0) {
        riak_error err = riak_object_new_array(ctx, &(response->content), rpbresp->n_content);
        if (err != ERIAK_OK) {
            riak_free(ctx, response);
            return 1;
        }
        response->n_content = rpbresp->n_content;
        for(i = 0; i < rpbresp->n_content; i++) {
            err = riak_object_new_from_pb(ctx, &(response->content[i]), rpbresp->content[i]);
            // If any object allocation fails, clean up all previously allocated ones
            if (err != ERIAK_OK) {
                riak_object_free_array(ctx, &(response->content), i);
                riak_free(ctx, response);
                return err;
            }
        }
    }
    *resp = response;

    return ERIAK_OK;
}

void
riak_free_put_response(riak_event         *rev,
                       riak_put_response **resp) {
    riak_context *ctx = (riak_context*)(rev->context);
    riak_put_response *response = *resp;
    if (response->n_content > 0) {
        riak_object_free_array(ctx, &(response->content), response->n_content);
    }
    rpb_put_resp__free_unpacked(response->_internal, ctx->pb_allocator);
    riak_free_ptr(ctx, resp);
}

riak_error
riak_encode_delete_request(riak_event          *rev,
                           riak_binary         *bucket,
                           riak_binary         *key,
                           riak_delete_options *options,
                           riak_pb_message    **req) {

    riak_context *ctx = (riak_context*)(rev->context);
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
        return ERIAK_OUT_OF_MEMORY;
    }
    rpb_del_req__pack (&delmsg, msgbuf);

    riak_pb_message* request = riak_pb_message_new(ctx, MSG_RPBDELREQ, msglen, msgbuf);
    if (request == NULL) {
        return ERIAK_OUT_OF_MEMORY;
    }
    *req = request;
    riak_event_set_response_decoder(rev, (riak_response_decoder)riak_decode_delete_response);

    return ERIAK_OK;
}


riak_error
riak_decode_delete_response(riak_event            *rev,
                            riak_pb_message       *pbresp,
                            riak_delete_response **resp,
                            riak_boolean_t        *done) {
    riak_context *ctx = (riak_context*)(rev->context);
    riak_delete_response *response = (riak_delete_response*)(ctx->malloc_fn)(sizeof(riak_delete_response));
    *done = RIAK_TRUE;
    if (response == NULL) {
        return ERIAK_OUT_OF_MEMORY;
    }
    *resp = response;

    return ERIAK_OK;
}

void
riak_free_delete_response(riak_event            *rev,
                          riak_delete_response **resp) {
    riak_context *ctx = (riak_context*)(rev->context);
    riak_free_ptr(ctx, resp);
}

riak_error
riak_encode_listbuckets_request(riak_event       *rev,
                                riak_pb_message **req) {
    riak_context *ctx = (riak_context*)(rev->context);
    RpbListBucketsReq listbucketsreq = RPB_LIST_BUCKETS_REQ__INIT;
    listbucketsreq.stream = RIAK_TRUE;
    listbucketsreq.has_stream = RIAK_TRUE;
    riak_size_t msglen = rpb_list_buckets_req__get_packed_size(&listbucketsreq);
    riak_uint8_t *msgbuf = (riak_uint8_t*)(ctx->malloc_fn)(msglen);
    if (msgbuf == NULL) {
        return ERIAK_OUT_OF_MEMORY;
    }
    rpb_list_buckets_req__pack(&listbucketsreq, msgbuf);
    riak_pb_message* request = riak_pb_message_new(ctx, MSG_RPBLISTBUCKETSREQ, msglen, msgbuf);
    if (request == NULL) {
        riak_free(ctx, msgbuf);
        return ERIAK_OUT_OF_MEMORY;
    }
    *req = request;
    riak_event_set_response_decoder(rev, (riak_response_decoder)riak_decode_listbuckets_response);

    return ERIAK_OK;
}


riak_error
riak_decode_listbuckets_response(riak_event                 *rev,
                                 riak_pb_message            *pbresp,
                                 riak_listbuckets_response **resp,
                                 riak_boolean_t             *done) {
    riak_context *ctx = rev->context;
    riak_log(rev, RIAK_LOG_DEBUG, "riak_decode_listbuckets_response");
    RpbListBucketsResp *listbucketresp = rpb_list_buckets_resp__unpack(ctx->pb_allocator, (pbresp->len)-1, (uint8_t*)((pbresp->data)+1));
    int i;
    riak_listbuckets_response *response = (ctx->malloc_fn)(sizeof(riak_listbuckets_response));
    response->buckets = (riak_binary**)(ctx->malloc_fn)(sizeof(riak_binary*)*listbucketresp->n_buckets);
    if (response->buckets == NULL) {
        return ERIAK_OUT_OF_MEMORY;
    }
    response->n_buckets = listbucketresp->n_buckets;
    for(i = 0; i < listbucketresp->n_buckets; i++) {
        ProtobufCBinaryData binary = listbucketresp->buckets[i];
        response->buckets[i] = riak_binary_new(ctx, binary.len, binary.data);
        if (response->buckets[i] == NULL) {
            int j;
            rpb_list_buckets_resp__free_unpacked(listbucketresp, ctx->pb_allocator);
            for(j = 0; j < i; j++) {
                riak_free(ctx, response->buckets[j]);
            }
            riak_free(ctx, response->buckets);
            riak_free(ctx, response);
            return ERIAK_OUT_OF_MEMORY;
        }
    }
    response->done = RIAK_FALSE;
    if (listbucketresp->has_done == RIAK_TRUE) {
        riak_log(rev, RIAK_LOG_DEBUG, "HAS DONE");
        response->done = listbucketresp->done;
    }
    response->_internal = listbucketresp;
    *resp = response;
    *done = response->done;

    return ERIAK_OK;
}


void
riak_free_listbuckets_response(riak_event                 *rev,
                               riak_listbuckets_response **resp) {
    riak_context *ctx = (riak_context*)(rev->context);
    riak_listbuckets_response *response = *resp;
    int i;
    for(i = 0; i < response->n_buckets; i++) {
        riak_free(ctx, response->buckets[i]);
    }
    riak_free(ctx, response->buckets);
    rpb_list_buckets_resp__free_unpacked(response->_internal, ctx->pb_allocator);
    riak_free_ptr(ctx, resp);
}


riak_error
riak_encode_listkeys_request(riak_event       *rev,
                             riak_binary      *bucket,
                             riak_uint32_t     timeout,
                             riak_pb_message **req) {
    riak_context *ctx = (riak_context*)(rev->context);
    RpbListKeysReq listkeysreq = RPB_LIST_KEYS_REQ__INIT;

    riak_binary_to_pb_copy_ptr(&(listkeysreq.bucket), bucket);
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

    riak_pb_message* request = riak_pb_message_new(ctx, MSG_RPBLISTKEYSREQ, msglen, msgbuf);
    if (request == NULL) {
        return ERIAK_OUT_OF_MEMORY;
    }
    *req = request;
    riak_event_set_response_decoder(rev, (riak_response_decoder)riak_decode_listkeys_response);

    return ERIAK_OK;

}

riak_error
riak_decode_listkeys_response(riak_event              *rev,
                              riak_pb_message         *pbresp,
                              riak_listkeys_response **resp,
                              riak_boolean_t          *done) {
    riak_context *ctx = rev->context;
    riak_log(rev, RIAK_LOG_DEBUG, "riak_decode_listkeys_response");
    RpbListKeysResp *listkeyresp = rpb_list_keys_resp__unpack(ctx->pb_allocator, (pbresp->len)-1, (uint8_t*)((pbresp->data)+1));
    int i;
    riak_listkeys_response *response = (ctx->malloc_fn)(sizeof(riak_listkeys_response));
    response->keys = (riak_binary**)(ctx->malloc_fn)(sizeof(riak_binary)*(listkeyresp->n_keys));
    if (response->keys == NULL) {
        return ERIAK_OUT_OF_MEMORY;
    }
    response->n_keys = listkeyresp->n_keys;
    for(i = 0; i < listkeyresp->n_keys; i++) {
        ProtobufCBinaryData binary = listkeyresp->keys[i];
        response->keys[i] = riak_binary_new(ctx, binary.len, binary.data);
        if (response->keys[i]->data == NULL) {
            int j;
            rpb_list_keys_resp__free_unpacked(listkeyresp, ctx->pb_allocator);
            for(j = 0; j < i; j++) {
                riak_free(ctx, response->keys[j]);
            }
            riak_free(ctx, response->keys);
            riak_free(ctx, response);

            return ERIAK_OUT_OF_MEMORY;
        }
    }
    response->done = RIAK_FALSE;
    if (listkeyresp->has_done) {
        riak_log(rev, RIAK_LOG_DEBUG, "HAS DONE");
        response->done = listkeyresp->done;
    }
    *done = response->done;
    *resp = response;

    return ERIAK_OK;
}

void
riak_free_listkeys_response(riak_event                 *rev,
                               riak_listkeys_response **resp) {
    riak_context *ctx = (riak_context*)(rev->context);
    riak_listkeys_response *response = *resp;
    int i;
    for(i = 0; i < response->n_keys; i++) {
        riak_free(ctx, response->keys[i]);
    }
    riak_free(ctx, response->keys);
    rpb_list_keys_resp__free_unpacked(response->_internal, ctx->pb_allocator);
    riak_free_ptr(ctx, resp);
}

/* LIBEVENT CALLBACKS */

void
riak_event_callback(riak_bufferevent *bev,
                    short             events,
                    void             *ptr)
{
    riak_event   *rev = (riak_event*)ptr;
    if (events & BEV_EVENT_CONNECTED) {
         riak_log(rev, RIAK_LOG_DEBUG, "Connect okay.");
    } else if (events & (BEV_EVENT_ERROR|BEV_EVENT_EOF)) {
         char *reason = "BEV_EVENT_ERROR";
         if (events & BEV_EVENT_ERROR) {
            reason = "BEV_EVENT_EOF";
            int err = bufferevent_socket_get_dns_error(bev);
            if (err)
                riak_log(rev, RIAK_LOG_ERROR, "DNS error: %s", evutil_gai_strerror(err));
         }
         struct evbuffer *ev_read  = bufferevent_get_input(bev);
         struct evbuffer *ev_write = bufferevent_get_output(bev);
         riak_log(rev, RIAK_LOG_DEBUG, "Closing because of %s [read event=%p, write event=%p]",
                 reason, (void*)ev_read, (void*)ev_write);
         bufferevent_free(bev);
         event_base_loopexit(rev->base, NULL);
    } else if (events & BEV_EVENT_TIMEOUT) {
        riak_log(rev, RIAK_LOG_DEBUG, "Timeout Event");
        bufferevent_free(bev);
        event_base_loopexit(rev->base, NULL);
    } else {
        riak_log(rev, RIAK_LOG_DEBUG, "Event %d", events);
    }
}


// MAIN RESULT CALLBACK
void
riak_read_result_callback(riak_bufferevent *bev,
                          void             *ptr) {
    riak_event    *rev = (riak_event*)ptr;
    riak_context  *ctx = (riak_context*)(rev->context);
    riak_boolean_t done_streaming = RIAK_FALSE;
    riak_size_t    buflen;
    while(RIAK_TRUE) {
        // Are we in the middle of a message already?
        if (rev->msglen_complete == RIAK_FALSE) {
            // Read the first 32-bits which are the message size
            // However, if a few size bytes were included during the last read, add them in
            riak_uint32_t  inmsglen = rev->msglen;
            riak_size_t remaining_msg_len = sizeof(inmsglen) - rev->position;
            riak_uint8_t *target = (riak_uint8_t*)(&inmsglen);
            target += rev->position;
            buflen = bufferevent_read(bev, target, remaining_msg_len);
            target = (riak_uint8_t*)(&inmsglen);
            // If we can't ready any more bytes, stop trying
            if (buflen != remaining_msg_len) {
                riak_log(rev, RIAK_LOG_DEBUG, "Expected %d bytes but received bytes = %d", remaining_msg_len, buflen);
                if (buflen == 0) break;
                // A few message size bytes of next message were in this buffer
                // Stuff the partial size into rev->msglen and note the position
                if (buflen < sizeof(inmsglen)) {
                    rev->position = buflen;
                    rev->msglen = inmsglen;
                    return;
                }
                abort();  // Something is hosed here
            }

            rev->msglen_complete = RIAK_TRUE;
            rev->msglen = ntohl(inmsglen);
            riak_log(rev, RIAK_LOG_DEBUG, "Read msglen = %d", rev->msglen);

            // TODO: Need to malloc new buffer each time?
            rev->msgbuf = (riak_uint8_t*)(ctx->malloc_fn)(rev->msglen);
            if (rev->msgbuf == NULL) {
                riak_log(rev, RIAK_LOG_FATAL, "Could not allocate buffer in riak_read_result_callback");
                abort();
            }
        } else {
            riak_log(rev, RIAK_LOG_DEBUG, "Continuation of partial message");
        }

        riak_uint8_t *current_position = rev->msgbuf;
        current_position += rev->position;
        buflen = bufferevent_read(bev, (void*)current_position, rev->msglen - rev->position);
        riak_log(rev, RIAK_LOG_DEBUG, "read %d bytes at position %d, msglen = %d", buflen, rev->position, rev->msglen);
        rev->position += buflen;
        // Are we done yet? If not, break out and wait for the next callback
        if (rev->position < rev->msglen) {
            riak_log(rev, RIAK_LOG_DEBUG, "Partial message received");
            return;
        }
        assert(rev->position == rev->msglen);

        riak_uint8_t msgid = (rev->msgbuf)[0];
        riak_pb_message *pbresp = riak_pb_message_new(ctx, msgid, rev->msglen, rev->msgbuf);
        riak_error result;
        rev->position = 0;  // Reset on success
        rev->msglen = 0;
        rev->msglen_complete = RIAK_FALSE;

        // Response varies by data type
        void *response = NULL;
        riak_error_response *err_response = NULL;
        // Assume we are doing a single loop, unless told otherwise
        done_streaming = RIAK_TRUE;
        if (rev->decoder == NULL) {
            riak_log(rev, RIAK_LOG_FATAL, "%d NOT IMPLEMENTED", msgid);
            abort();
        }
        if (msgid == MSG_RPBERRORRESP) {
            result = riak_decode_error_response(rev, pbresp, &err_response, &done_streaming);
            riak_log(rev, RIAK_LOG_FATAL, "ERR #%d - %s\n", err_response->errcode, err_response->errmsg.data);
            if (rev->error_cb) (rev->error_cb)(err_response, rev->cb_data);
            exit(1);
        }
        // Decode the message from Protocol Buffers
        result = (rev->decoder)(rev, pbresp, &response, &done_streaming);

        // Call the user-defined callback for this message
        if (rev->response_cb) (rev->response_cb)(response, rev->cb_data);

        // NOTE: Also frees the local buffer
        riak_pb_message_free(ctx, &pbresp);

        // Something is amiss
        if (result)
            abort();
    }

    // What has been queued up
    fflush(stdout);
    event_base_dump_events(rev->base, stdout);

    if (done_streaming)
        bufferevent_free(bev);

    //sleep(1);
}
