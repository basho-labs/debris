/*********************************************************************
 *
 * riak_pb_message.h: Riak C Client Protocol Buffer Message
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

#ifndef RIAK_PB_MESSAGE_H
#define RIAK_PB_MESSAGE_H

#include "riak.h"
#include "riak_kv.pb-c.h"

#define MSG_RPBERRORRESP              0

// 0 length
#define MSG_RPBPINGREQ                1

// 0 length
#define MSG_RPBPINGRESP               2

#define MSG_RPBGETCLIENTIDREQ         3

// 0 length
#define MSG_RPBGETCLIENTIDRESP        4

// 0 length
#define MSG_RPBSETCLIENTIDREQ         5
#define MSG_RPBSETCLIENTIDRESP        6
#define MSG_RPBGETSERVERINFOREQ       7
#define MSG_RPBGETSERVERINFORESP      8
#define MSG_RPBGETREQ                 9
#define MSG_RPBGETRESP                10
#define MSG_RPBPUTREQ                 11
#define MSG_RPBPUTRESP                12
#define MSG_RPBDELREQ                 13
#define MSG_RPBDELRESP                14
#define MSG_RPBLISTBUCKETSREQ         15

// streaming
#define MSG_RPBLISTBUCKETSRESP        16
#define MSG_RPBLISTKEYSREQ            17

// streaming
#define MSG_RPBLISTKEYSRESP           18

#define MSG_RPBGETBUCKETREQ           19
#define MSG_RPBGETBUCKETRESP          20
#define MSG_RPBSETBUCKETREQ           21
#define MSG_RPBSETBUCKETRESP          22
#define MSG_RPBMAPREDREQ              23

// streaming
#define MSG_RPBMAPREDRESP             24

#define MSG_RPBINDEXREQ               25
#define MSG_RPBINDEXRESP              26
#define MSG_RPBSEARCHQUERYREQ         27
#define MSG_RBPSEARCHQUERYRESP        28
#define MSG_RPBRESETBUCKETREQ         29
#define MSG_RPBRESETBUCKETRESP        30
#define MSG_RPBGETBUCKETTYPEREQ       31
#define MSG_RPBSETBUCKETTYPEREQ       32
#define MSG_RPBRESETBUCKETTYPEREQ     33
#define MSG_RPBCSBUCKETREQ            40
#define MSG_RPBCSBUCKETRESP           41
#define MSG_RPBCOUNTERUPDATEREQ       50
#define MSG_RPBCOUNTERUPDATERESP      51
#define MSG_RPBCOUNTERGETREQ          52
#define MSG_RPBCOUNTERGETRESP         53
// Yokozuna Search
#define MSG_RPBYOKOZUNAINDEXGETREQ    54
#define MSG_RPBYOKOZUNAINDEXGETRESP   55
#define MSG_RPBYOKOZUNAINDEXPUTREQ    56
#define MSG_RPBYOKOZUNAINDEXDELETEREQ 57
#define MSG_RPBYOKOZUNASCHEMAGETREQ   58
#define MSG_RPBYOKOZUNASCHEMAGETRESP  59
#define MSG_RPBYOKOZUNASCHEMAPUTREQ   60
// CRDTs
#define MSG_DTFETCHREQ                80
#define MSG_DTFETCHRESP               81
#define MSG_DTUPDATEREQ               82
#define MSG_DTUPDATERESP              83
// internal message codes, grow downwards from 255
#define MSG_RPBAUTHREQ                253
#define MSG_RPBAUTHRESP               254
#define MSG_RPBSTARTTLS               255

typedef struct _riak_pb_message {
    riak_uint32_t len;
    riak_uint8_t  msgid;
    riak_uint8_t *data;
} riak_pb_message;

riak_pb_message*
riak_pb_message_new(riak_context *ctx,
                    riak_uint8_t  msgtype,
                    riak_size_t   msglen,
                    riak_uint8_t *buffer);
void
riak_pb_message_free(riak_context     *ctx,
                     riak_pb_message **pb);

/**
 * @brief Convert PBC error response into user-readable data type
 * @param rev Riak Event
 * @param pbresp Protocol Buffer response from Riak
 * @param resp Returned error structure
 * @param done Returned flag set to true if finished streaming
 * @return Error if out of memory
 */
riak_error
riak_decode_error_response(riak_event           *rev,
                           riak_pb_message      *pbresp,
                           riak_error_response **resp,
                           riak_boolean_t       *done);
/**
 * @brief Free memory used by an error response
 * @param ctx Riak Context
 * @param resp Error structure to be freed
 */
void
riak_free_error_response(riak_context         *ctx,
                         riak_error_response **resp);

/**
 * @brief Build a ping request
 * @param rev Riak Event
 * @param req Created PB message
 * @return Error if out of memory
 */
riak_error
riak_encode_ping_request(riak_event       *rev,
                         riak_pb_message **req);

/**
 * @brief Convert PBC error response into user-readable data type
 * @param rev Riak Event
 * @param pbresp Protocol Buffer response from Riak
 * @param resp Returned error structure
 * @param done Returned flag set to true if finished streaming
 * @return Error if out of memory
 */
riak_error
riak_decode_ping_response(riak_event          *rev,
                          riak_pb_message     *pbresp,
                          riak_ping_response **resp,
                          riak_boolean_t      *done);

/**
 * @brief Free memory from response
 * @param ctx Riak Context
 * @param resp Ping PBC Response
 */
void
riak_free_ping_response(riak_context        *ctx,
                        riak_ping_response **resp);

/**
 * @brief Build a server info request
 * @param rev Riak Event
 * @param req Created PB message
 * @return Error if out of memory
 */
riak_error
riak_encode_serverinfo_request(riak_event       *rev,
                               riak_pb_message **req);

/**
 * @brief Translate PBC message to Riak message
 * @param rev Riak Event
 * @param pbresp Protocol Buffer message
 * @param done Returned flag set to true if finished streaming
 * @param resp Returned Get message
 * @return Error if out of memory
 */
riak_error
riak_decode_serverinfo_response(riak_event                *rev,
                                riak_pb_message           *pbresp,
                                riak_serverinfo_response **resp,
                                riak_boolean_t            *done);

/**
 * @brief Print a summary of a `riak_serverinfo_response`
 * @param response Result from a Server Info request
 * @param target Location of string to be formatted
 * @param len Number of free bytes
 */
void
riak_print_serverinfo_response(riak_serverinfo_response *response,
                               char                     *target,
                               riak_size_t               len);

/**
 * @brief Free memory from response
 * @param ctx Riak Context
 * @param resp Server Info PBC Response
 */
void
riak_free_serverinfo_response(riak_context              *ctx,
                              riak_serverinfo_response **resp);


/**
 * @brief Create a get/fetch Request
 * @param rev Riak Event
 * @param bucket Name of Riak bucket
 * @param key Name of Riak key
 * @param options Get request parameters
 * @param req Returned PBC request
 * @return Error if out of memory
 */
riak_error
riak_encode_get_request(riak_event       *rev,
                        riak_binary      *bucket,
                        riak_binary      *key,
                        riak_get_options *options,
                        riak_pb_message **req);

/**
 * @brief Translate PBC message to Riak message
 * @param rev Riak Event
 * @param pbresp Protocol Buffer message
 * @param done Returned flag set to true if finished streaming
 * @param resp Returned Get message
 * @return Error if out of memory
 */
riak_error
riak_decode_get_response(riak_event         *rev,
                         riak_pb_message    *pbresp,
                         riak_get_response **resp,
                         riak_boolean_t     *done);

/**
 * @brief Print a summary of a `riak_get_response`
 * @param response Result from a Get request
 * @param target Location of string to be formatted
 * @param len Number of free bytes
 */
void
riak_print_get_response(riak_get_response *response,
                        char              *target,
                        riak_size_t        len);

/**
 * @brief Free get response
 * @param ctx Riak Context
 * @param resp Get response
 */
void
riak_free_get_response(riak_context       *ctx,
                       riak_get_response **resp);

/**
 * @brief Create Put Request
 * @param rev Riak Event
 * @param riak_obj Riak object to be put
 * @param options Options to the put request
 * @param req Returned request message
 * @return Error if out of memory
 */
riak_error
riak_encode_put_request(riak_event       *rev,
                        riak_object      *riak_obj,
                        riak_put_options *options,
                        riak_pb_message **req);

/**
 * @brief Translate PBC put message to a Riak response
 * @param rev Riak Event
 * @param pbresp Protocol Buffer message
 * @param resp Returned Put message
 * @param done Returned flag set to true if finished streaming
 * @return Error if out of memory
 */
riak_error
riak_decode_put_response(riak_event         *rev,
                         riak_pb_message    *pbresp,
                         riak_put_response **resp,
                         riak_boolean_t     *done);

/**
 * @brief Print a summary of a `riak_put_response`
 * @param response Result from a Put request
 * @param target Location of string to be formatted
 * @param len Number of free bytes
 */
void
riak_print_put_response(riak_put_response *response,
                        char              *target,
                        riak_size_t        len);

/**
 * @brief Free put response
 * @param ctx Riak Context
 * @param resp Put message to be cleaned up
 */
void
riak_free_put_response(riak_context       *ctx,
                       riak_put_response **resp);

/**
 * @brief Create a deletion request
 * @param bucket Name of Riak bucket
 * @param key Name of Riak key
 * @param options Delete request parameters
 * @param req Returned PBC request
 * @return Error if out of memory
 */
riak_error
riak_encode_delete_request(riak_event          *rev,
                           riak_binary         *bucket,
                           riak_binary         *key,
                           riak_delete_options *options,
                           riak_pb_message    **req);


/**
 * @brief Translate PBC delete message to a Riak response
 * @param rev Riak Event
 * @param pbresp Protocol Buffer message
 * @param resp Returned Delete message
 * @param done Returned flag set to true if finished streaming
 * @return Error if out of memory
 */
riak_error
riak_decode_delete_response(riak_event            *rev,
                            riak_pb_message       *pbresp,
                            riak_delete_response **resp,
                            riak_boolean_t        *done);

/**
 * @brief Free memory from response
 * @param ctx Riak Context
 * @param resp Delete PBC Response
 */
void
riak_free_delete_response(riak_context          *ctx,
                          riak_delete_response **resp);

/**
 * @brief Create a request to find all buckets
 * @param rev Riak Event
 * @param req Returned listbuckets request
 * @return Error if out of memory
 */
riak_error
riak_encode_listbuckets_request(riak_event       *rev,
                                riak_pb_message **req);

/**
 * @brief Translate PBC listbuckets response into Riak strucuture
 * @param rev Riak Event
 * @param pbresp PBC response message
 * @param resp Returned Riak response structure
 * @param done Returned flag set to true if finished streaming
 * @return Error if out of memory
 */
riak_error
riak_decode_listbuckets_response(riak_event                 *rev,
                                 riak_pb_message            *pbresp,
                                 riak_listbuckets_response **resp,
                                 riak_boolean_t             *done);

/**
 * @brief Print a summary of a `riak_listbuckets_response`
 * @param response Result from a List buckets request
 * @param target Location of string to be formatted
 * @param len Number of free bytes
 */
void
riak_print_listbuckets_response(riak_listbuckets_response *response,
                                char                      *target,
                                riak_size_t                len);
/**
 * @brief Free listbuckets response
 * @param ctx Riak Context
 * @param resp List buckets message to be cleaned up
 */
void
riak_free_listbuckets_response(riak_context               *ctx,
                               riak_listbuckets_response **resp);

/**
 * @brief Create a request to find all keys in a bucket
 * @param rev Riak Event
 * @param bucket Name of Riak bucket
 * @param timeout How long to wait for a response
 * @param req Returned listbuckets request
 * @return Error if out of memory
 */
riak_error
riak_encode_listkeys_request(riak_event       *rev,
                                 riak_binary  *bucket,
                                 riak_uint32_t timeout,
                                 riak_pb_message **req);

/**
 * @brief Translate PBC listkeys response into Riak structure
 * @param rev Riak Event
 * @param pbresp PBC response message
 * @param resp Returned Riak response structure
 * @param done Returned flag set to true if finished streaming
 * @return Error if out of memory
 */
riak_error
riak_decode_listkeys_response(riak_event              *rev,
                              riak_pb_message         *pbresp,
                              riak_listkeys_response **resp,
                              riak_boolean_t          *done);

/**
 * @brief Print a summary of a `riak_listkeys_response`
 * @param response Result from a Listkeys request
 * @param target Location of string to be formatted
 * @param len Number of free bytes
 */
void
riak_print_listkeys_response(riak_listkeys_response *response,
                             char                   *target,
                             riak_size_t             len);

/**
 * @brief Free listkeys response
 * @param ctx Riak Context
 * @param resp List keys message to be cleaned up
 */
void
riak_free_listkeys_response(riak_context            *ctx,
                            riak_listkeys_response **resp);

/**
 * @brief Build a client id request
 * @param rev Riak Event
 * @param req Created PB message
 * @return Error if out of memory
 */
riak_error
riak_encode_get_clientid_request(riak_event       *rev,
                                 riak_pb_message **req);

/**
 * @brief Translate PBC message to Riak message
 * @param rev Riak Event
 * @param pbresp Protocol Buffer message
 * @param done Returned flag set to true if finished streaming
 * @param resp Returned Get message
 * @return Error if out of memory
 */
riak_error
riak_decode_get_clientid_response(riak_event                  *rev,
                                  riak_pb_message             *pbresp,
                                  riak_get_clientid_response **resp,
                                  riak_boolean_t              *done);

/**
 * @brief Print a summary of a `riak_get_clientid_response`
 * @param response Result from a client id request
 * @param target Location of string to be formatted
 * @param len Number of free bytes
 */
void
riak_print_get_clientid_response(riak_get_clientid_response *response,
                                 char                       *target,
                                 riak_size_t                 len);

/**
 * @brief Free memory from response
 * @param ctx Riak Context
 * @param resp client id PBC Response
 */
void
riak_free_get_clientid_response(riak_context              *ctx,
                              riak_get_clientid_response **resp);

/**
 * @brief Build a client id request
 * @param rev Riak Event
 * @param clientid Client id for current connection
 * @param req Created PB message
 * @return Error if out of memory
 */
riak_error
riak_encode_set_clientid_request(riak_event       *rev,
                                 riak_binary      *clientid,
                                 riak_pb_message **req);

/**
 * @brief Translate PBC message to Riak message
 * @param rev Riak Event
 * @param pbresp Protocol Buffer message
 * @param done Returned flag set to true if finished streaming
 * @param resp Returned set message
 * @return Error if out of memory
 */
riak_error
riak_decode_set_clientid_response(riak_event                  *rev,
                                  riak_pb_message             *pbresp,
                                  riak_set_clientid_response **resp,
                                  riak_boolean_t              *done);

/**
 * @brief Free memory from response
 * @param ctx Riak Context
 * @param resp client id PBC Response
 */
void
riak_free_set_clientid_response(riak_context              *ctx,
                              riak_set_clientid_response **resp);


#endif
