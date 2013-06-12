#include "riak_pb_messages.h"
#include "riak.h"
#include "riak.pb-c.h"
#include "riak_kv.pb-c.h"
#include "riak_utils.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct pb_request* pb_encode_get_request(struct riak_context *ctx,
                                         struct riak_binary *bucket,
                                         struct riak_binary *key,
                                         struct riak_get_options* get_options) {
   void *msgbuf;
   unsigned msglen;

   RpbGetReq getmsg = RPB_GET_REQ__INIT;

   // probably incorrect, need to memcopy the bucket and key
   getmsg.bucket.data = bucket->data;
   getmsg.bucket.len = bucket->len;

   getmsg.key.data = key->data;
   getmsg.key.len = key->len;


   // process get options
   if(get_options != 0) {
     // TODO
   }
   msglen = rpb_get_req__get_packed_size (&getmsg);
   msgbuf = malloc (msglen);
   rpb_get_req__pack (&getmsg, msgbuf);

   struct pb_request *request = malloc(sizeof(struct pb_request));
   request->reqid = MSG_RPBGETREQ;
   request->msglength = msglen;
   request->reqdata = msgbuf;
   return request;
}

void* pb_decode_get_response(struct riak_context *ctx,
                             struct pb_response *raw_response) {
  // decode the PB response etc
     struct riak_response *response = malloc(sizeof(struct riak_response));

     RpbGetResp *getresp = rpb_get_resp__unpack(NULL, raw_response->msglength, raw_response->respdata);
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
     // TODO: something will have to free raw_response and the buffer inside of it
}



