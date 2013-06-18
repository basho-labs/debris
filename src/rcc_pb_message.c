#include "rcc.h"
#include "rcc_pb_message.h"
#include "rcc_utils.h"
#include "riak.pb-c.h"
#include "riak_kv.pb-c.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int pb_encode_get_request(struct riak_context *ctx,
                          struct riak_binary *bucket,
                          struct riak_binary *key,
                          struct riak_get_options* get_options,
                          struct pb_request *pb_req) {
   void *msgbuf;
   unsigned msglen;

   RpbGetReq getmsg = RPB_GET_REQ__INIT;

   getmsg.bucket.data = bucket->data;
   getmsg.bucket.len = bucket->len;

   getmsg.key.data = key->data;
   getmsg.key.len = key->len;

   // process get options
   if(get_options != 0) {
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


int pb_decode_get_response(struct riak_context *ctx,
                           struct pb_response *pbresp,
                           struct riak_response *response) {
  // decode the PB response etc
     RpbGetResp *getresp = rpb_get_resp__unpack(NULL, pbresp->msglength, pbresp->respdata);
     int i = 0;
     if(getresp->n_content > 0) {
       response->objects = malloc(sizeof(struct riak_object) * getresp->n_content);
       response->object_count = getresp->n_content;
       for(i = 0; i < getresp->n_content; i++) {
         RpbContent *c = getresp->content[i];
         populate_riak_binary(&(response->objects[i].value), c->value.len, c->value.data);
       }
     }
     rpb_get_resp__free_unpacked(getresp, NULL);
     // TODO: something will have to free pb_resp and the buffer inside of it
    return 0;
}



