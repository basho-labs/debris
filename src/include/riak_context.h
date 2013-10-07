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
#define RIAK_HOST_MAX_LEN               256

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
    riak_event_base    *base; // Is this the right location? One per thread, so probably
    char                hostname[RIAK_HOST_MAX_LEN];
    char                portnum[RIAK_HOST_MAX_LEN]; // Keep as a string for debugging
    riak_addrinfo      *addrinfo;
} riak_context;


/**
 * @brief Construct a Riak Context
 * @param context Spanking new `riak_context` struct
 * @param alloc Memory allocator function (optional)
 * @param realloc Memory re-allocation function (optional)
 * @param freeme Memory releasing function (optional)
 * @param pb_alloc Memory allocator function for protocol buffers (optional)
 * @param pb_free Memory releasing function for protocol buffer (optional)
 * @param logging_category logging prefix (optional)
 * @param hostname Name of Riak server
 * @param portnum Riak PBC port number
 * @return Error code
 */
riak_error
riak_context_new(riak_context    **context,
                 const char       *hostname,
                 const char       *portnum,
                 riak_alloc_fn     alloc,
                 riak_realloc_fn   realloc,
                 riak_free_fn      freeme,
                 riak_pb_alloc_fn  pb_alloc,
                 riak_pb_free_fn   pb_free,
                 const char       *logging_category);

/**
 * @brief Construct a Riak Context with system memory
 * management utils * (malloc/free)
 * @param context Spanking new `riak_context` struct
 * @param hostname Name of Riak server
 * @param portnum Riak PBC port number
 * @return Error code
 */
riak_error
riak_context_new_default(riak_context** context,
                         char* hostname,
                         char* portnum);

/**
 * @brief Gets the underlying event base
 * @param ctx Riak Context
 * @return Return event base
 */
riak_event_base*
riak_context_get_base(riak_context *ctx);

/**
 * @brief Reclaim memory used by a `riak_context`
 * @param ctx Context struct
 */
void
riak_context_free(riak_context **ctx);

#endif /* RIAK_CONTEXT_H_ */
