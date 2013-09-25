/*********************************************************************
 *
 * riak_event.h: Management of the Riak event
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

#ifndef RIAK_EVENT_H_
#define RIAK_EVENT_H_

// Generic placeholder for message-specific callbacks
typedef void (*riak_response_callback)(void *response, void *ptr);

// Forward declarations
struct _riak_event;
struct _riak_pb_message;

// Template for message-specific decoder
typedef riak_error (*riak_response_decoder)(struct _riak_event      *rev,
                                            struct _riak_pb_message *pbresp,
                                            void                   **response,
                                            riak_boolean_t          *done);

// Essentially the state of the current event
typedef struct _riak_event {
    riak_context            *context;
    riak_event_base         *base;
    riak_bufferevent        *bevent;
    struct _riak_pb_message *request;
    riak_response_decoder    decoder;
    riak_response_callback   response_cb;
    riak_response_callback   error_cb;
    void                    *cb_data;
    riak_socket_t            fd;

    // Current message being decoded
    riak_uint32_t            position;
    riak_uint32_t            msglen;
    riak_uint8_t            *msgbuf;
    riak_boolean_t           msglen_complete;
} riak_event;


/**
 * @brief Construct a Riak event
 * @param ctx Riak context for memory allocation
 * @param decoder Pointer to function to decode response
 * @param response_cb Reaponse callback function (user-supplied)
 * @param cb_data Pointer passed to `response_cb` when it is called
 * @returns Spanking new `riak_event` struct
 */
riak_event*
riak_event_new(riak_context          *ctx,
               riak_response_decoder  decoder,
               riak_response_callback response_cb,
               riak_response_callback error_cb,
               void                  *cb_data);

/**
 * @brief Set the event's callback data
 * @param rev Riak Event
 * @param cb_data Pointer to data used in user's callback
 */
void
riak_event_set_cb_data(riak_event *rev,
                       void       *cb_data);

/**
 * @brief Set the event's response callback
 * @param rev Riak Event
 * @param cb Function pointer to response callback
 */
void
riak_event_set_response_cb(riak_event             *rev,
                           riak_response_callback  cb);

/**
 * @brief Set the event's error callback
 * @param rev Riak Event
 * @param cb Function pointer to error callback
 */
void
riak_event_set_error_cb(riak_event             *rev,
                        riak_response_callback  cb);

/**
 * @brief Set the event's message decoding function
 * @param rev Riak Event
 * @param decoder Function pointer to message translator
 */
void
riak_event_set_response_decoder(riak_event             *rev,
                                riak_response_decoder   decoder);

/**
 * @brief Cleanup memory used by a Riak Event
 * @param re Riak Event
 */
void
riak_event_free(riak_event** re);

#endif
