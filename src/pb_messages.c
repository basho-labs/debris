#include "pb_messages.h"
#include "pb_transport.h"
#include "riak.h"
#include "riak.pb-c.h"
#include "riak_kv.pb-c.h"

#include <stdio.h>
#include <stdlib.h>

struct riak_response* riak_pb_get(struct riak_pb_transport *pbtransport,
                                  struct riak_string *bucket,
                                  struct riak_string *key) {
   void *msgbuf;
   unsigned msglen;

   RpbGetReq getmsg = RPB_GET_REQ__INIT;

   getmsg.bucket.data = bucket->data;
   getmsg.bucket.len = bucket->len;

   getmsg.key.data = key->data;
   getmsg.key.len = key->len;

   msglen = rpb_get_req__get_packed_size (&getmsg);
   msgbuf = malloc (msglen);
   rpb_get_req__pack (&getmsg, msgbuf);
   printf("riak_get [%s|%s = %i bytes]\n", bucket->data, key->data, msglen);

   struct pb_request request;
   request.reqid = MSG_RPBGETREQ;
   request.msglength = msglen;
   request.reqdata = msgbuf;
   pbtransport->send_message(pbtransport->transport_data, &request);
   struct pb_response response;
   response.expected_respid = MSG_RPBGETRESP;

   pbtransport->receive_message(pbtransport->transport_data, &response);

   //printf("Response = %s\n", (char*)response.respdata);
   //printf("Response length = %d\n", response.msglength);
   //printf("Response code = %d\n", response.actual_respid);

   // decode the PB response etc
   RpbGetResp *getresp = rpb_get_resp__unpack(NULL, response.msglength, (char*)response.respdata);
   printf("RPB values %d\n",getresp->n_content);
   if(getresp->n_content > 0) {
     RpbContent *c = getresp->content[0];
     printf("Value=[%s]\n", c->value.data);
   }
   rpb_get_resp__free_unpacked(getresp, NULL);
   free(response.respdata);
   return 0;
}
