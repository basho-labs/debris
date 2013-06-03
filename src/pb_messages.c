#include "pb_messages.h"
#include "pb_transport.h"
#include "riak.h"
#include "riak.pb-c.h"
#include "riak_kv.pb-c.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int riak_pb_get(struct riak_pb_transport *pbtransport,
                struct riak_binary *bucket,
                struct riak_binary *key,
                struct riak_get_options *options,
                struct riak_response *response) {
   void *msgbuf;
   unsigned msglen;

   RpbGetReq getmsg = RPB_GET_REQ__INIT;

   getmsg.bucket.data = bucket->data;
   getmsg.bucket.len = bucket->len;

   getmsg.key.data = key->data;
   getmsg.key.len = key->len;


   // process get options
   if(options != 0) {
     // TODO
   }
   msglen = rpb_get_req__get_packed_size (&getmsg);
   msgbuf = malloc (msglen);
   rpb_get_req__pack (&getmsg, msgbuf);

   struct pb_request request;
   request.reqid = MSG_RPBGETREQ;
   request.msglength = msglen;
   request.reqdata = msgbuf;
   pbtransport->send_message(pbtransport->transport_data, &request);
   free(msgbuf);

   struct pb_response pbresponse;
   pbresponse.expected_respid = MSG_RPBGETRESP;

   int pbr = pbtransport->receive_message(pbtransport->transport_data, &pbresponse);
   if(pbr >= 0) {
     // decode the PB response etc
     RpbGetResp *getresp = rpb_get_resp__unpack(NULL, pbresponse.msglength, pbresponse.respdata);
     int i = 0;
     if(getresp->n_content > 0) {
       response->objects = (struct riak_object*)malloc(sizeof(struct riak_object) * getresp->n_content);
       response->object_count = getresp->n_content;
       for(i = 0; i < getresp->n_content; i++) {
         RpbContent *c = getresp->content[i];
         populate_riak_binary(&(response->objects[i].value), c->value.len, c->value.data);
       }
     }
     rpb_get_resp__free_unpacked(getresp, NULL);
     free(pbresponse.respdata);
     return 0;
   } else {
     return -1;
   }
   // TODO: errors!
}


int riak_pb_put(struct riak_pb_transport *pbtransport,
                struct riak_object *riak_obj,
                struct riak_put_options *options,
                struct riak_response *response) {

   void *msgbuf;
   unsigned msglen;
   printf("riak_pb_get\n");
   RpbPutReq putmsg = RPB_PUT_REQ__INIT;

   putmsg.bucket.data = (uint8_t*)malloc(riak_obj->bucket.len);
   memcpy(&putmsg.bucket.data, &riak_obj->bucket.data, riak_obj->bucket.len);
   putmsg.bucket.len  = riak_obj->bucket.len;

   // TODO: key may not be present
   putmsg.has_key=1;
   putmsg.key.data = (uint8_t*)malloc(riak_obj->key.len);
   memcpy(&putmsg.key.data, &riak_obj->key.data, riak_obj->key.len);
   putmsg.key.len  = riak_obj->key.len;

   RpbContent *content = (RpbContent*)malloc(sizeof(RpbContent));
   rpb_content__init(content);
   putmsg.content = content;

   content->value.len  = riak_obj->value.len;
   content->value.data = (uint8_t*)malloc(riak_obj->value.len);
   memcpy(&putmsg.content->value.data, &riak_obj->value.data, riak_obj->value.len);

   // process get options
   if(options != 0) {
     // TODO
   }
   msglen = rpb_put_req__get_packed_size (&putmsg);
   msgbuf = malloc (msglen);
   rpb_put_req__pack (&putmsg, msgbuf);

   struct pb_request request;
   request.reqid = MSG_RPBPUTREQ;
   request.msglength = msglen;
   request.reqdata = msgbuf;
   pbtransport->send_message(pbtransport->transport_data, &request);
   free(msgbuf);

   struct pb_response pbresponse;
   pbresponse.expected_respid = MSG_RPBPUTRESP;

   int pbr = pbtransport->receive_message(pbtransport->transport_data, &pbresponse);
   if(pbr >= 0) {
     // decode the PB response etc
     RpbPutResp *putresp = rpb_put_resp__unpack(NULL, pbresponse.msglength, pbresponse.respdata);
     rpb_put_resp__free_unpacked(putresp, NULL);
     free(pbresponse.respdata);
     return 0;
   } else {
     return -1;
   }
   /// TODO: errors!
   return 0;
}


