#ifndef PB_MESSAGES_H
#define PB_MESSAGES_H

#include "riak.h"
#include "pb_transport.h"
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

int riak_pb_get(struct riak_pb_transport *,
                struct riak_string *bucket,
                struct riak_string *key,
                struct riak_response *);

#endif
