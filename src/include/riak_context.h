/*********************************************************************
 *
 * riak_context.h: Management of the Riak connection context
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

#ifndef RIAK_CONTEXT_H_
#define RIAK_CONTEXT_H_

// TODO: one day make this configurable?
#define RIAK_LOGGING_DEFAULT_CATEGORY   "com.basho.client.c"
#define RIAK_LOGGING_MAX_LEN            256

// per-thread
// do we need a *shared* context to complement?

typedef void *(*riak_alloc_fn)(size_t sz);
typedef void *(*riak_realloc_fn)(void *ptr, size_t size);
typedef void (*riak_free_fn)(void *ptr);
typedef void *(*riak_pb_alloc_fn)(void *allocator_data, size_t size);
typedef void (*riak_pb_free_fn)(void *allocator_data, void *pointer);

/* --- memory management --- */
//typedef struct _ProtobufCAllocator ProtobufCAllocator;
//struct _ProtobufCAllocator
//{
//  void *(*alloc)(void *allocator_data, size_t size);
//  void (*free)(void *allocator_data, void *pointer);
//  void *(*tmp_alloc)(void *allocator_data, size_t size);
//  unsigned max_alloca;
//  void *allocator_data;
//};

typedef struct _riak_context {
    riak_alloc_fn       malloc_fn;
    riak_realloc_fn     realloc_fn;
    riak_free_fn        free_fn;
    ProtobufCAllocator *pb_allocator;
    char                logging_category[RIAK_LOGGING_MAX_LEN];
} riak_context;


/**
 * @brief Construct a Riak Context
 * @param alloc Memory allocator function (optional)
 * @param realloc Memory re-allocation function (optional)
 * @param freeme Memory releasing function (optional)
 * @param pb_alloc Memory allocator function for protocol buffers (optional)
 * @param pb_free Memory releasing function for protocol buffer (optional)
 * @param logging_category logging prefix (optional)
 * @returns Spanking new `riak_context` struct
 */
riak_context*
riak_context_new(riak_alloc_fn alloc,
                 riak_realloc_fn realloc,
                 riak_free_fn freeme,
                 riak_pb_alloc_fn pb_alloc,
                 riak_pb_free_fn pb_free,
                 const char *logging_category);

// By default use system's built-in memory management utilities (malloc/free)
#define riak_context_new_default() riak_context_new(NULL,NULL,NULL,NULL,NULL,NULL)

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

/**
 * @brief Reclaim memory used by a `riak_context`
 * @param ctx Context struct
 */
void
riak_context_free(riak_context **ctx);

// Essentially the state of the current event
typedef struct _riak_event {
    riak_context          *context;
    riak_event_base       *base;
    riak_bufferevent      *bevent;
    riak_response_decoder  decoder;
    riak_response_callback response_cb;
    riak_response_callback error_cb;
    void                  *cb_data;
} riak_event;

/**
 * @brief Construct a Riak event
 * @param ctx Riak context for memory allocation
 * @param base Libevent event base
 * @param bev Libevent `bufferevent`
 * @param decoder Pointer to function to decode response
 * @param response_cb Reaponse callback function (user-supplied)
 * @param cb_data Pointer passed to `response_cb` when it is called
 * @returns Spanking new `riak_event` struct
 */
riak_event*
riak_event_new(riak_context          *ctx,
               riak_event_base       *base,
               riak_bufferevent      *bev,
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

#endif /* RIAK_CONTEXT_H_ */
