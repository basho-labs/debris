#ifndef RIAK_PB_MESSAGES_H
#define RIAK_PB_MESSAGES_H

#include "riak.h"

#define MSG_RPBERRORRESP 0

// 0 length
#define MSG_RPBPINGREQ 1

// 0 length
#define MSG_RPBPINGRESP 2

#define MSG_RPBGETCLIENTIDREQ 3
#define MSG_RPBGETCLIENTIDRESP 4
#define MSG_RPBSETCLIENTIDREQ 5
#define MSG_RPBSETCLIENTIDRESP 6
#define MSG_RPBGETSERVERINFOREQ 7
#define MSG_RPBGETSERVERINFORESP 8
#define MSG_RPBGETREQ 9
#define MSG_RPBGETRESP 10
#define MSG_RPBPUTREQ 11

// 0 length
#define MSG_RPBPUTRESP 12

#define MSG_RPBDELREQ 13
#define MSG_RPBDELRESP 14
#define MSG_RPBLISTBUCKETSREQ 15
#define MSG_RPBLISTBUCKETSRESP 16
#define MSG_RPBLISTKEYSREQ 17

// streaming
#define MSG_RPBLISTKEYSRESP 18

#define MSG_RPBGETBUCKETREQ 19
#define MSG_RPBGETBUCKETRESP 20
#define MSG_RPBSETBUCKETREQ 21
#define MSG_RPBSETBUCKETRESP 22
#define MSG_RPBMAPREDREQ 23

// streaming
#define MSG_RPBMAPREDRESP 24

#define MSG_RPBINDEXREQ 25
#define MSG_RPBINDEXRESP 26
#define MSG_RPBSEARCHQUERYREQ 27
#define MSG_RBPSEARCHQUERYRESP 28

struct pb_request {
  uint32_t msglength;
  uint8_t reqid;          // Protobuffs msg id for request
  void *reqdata;
};

struct pb_response {
  uint32_t msglength;
  uint8_t response_id; // TODO: need to do something for the error state
  void *respdata;
};

struct pb_request* pb_encode_get_request(struct riak_context *ctx,
                                         struct riak_binary *bucket,
                                         struct riak_binary *key,
                                         struct riak_get_options* opts);

void* pb_decode_get_response(struct riak_context *ctx,
                             struct pb_response*);
#endif
