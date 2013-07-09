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

// per-thread
// do we need a *shared* context to complement?

typedef     void *(*riak_alloc_fn)(size_t sz);
typedef     void *(*riak_realloc_fn)(void *ptr, size_t size);
typedef     void (*riak_free_fn)(void *ptr);
typedef     void *(*riak_pb_alloc_fn)(void *allocator_data, size_t size);
typedef     void (*riak_pb_free_fn)(void *allocator_data, void *pointer);

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
    riak_event_base    *base;
    riak_bufferevent   *bevent;
} riak_context;


/**
 * @brief Construct a Riak Context
 * @param base Libevent event base
 * @param bev Libevent `bufferevent`
 * @param alloc Memory allocator function (optional)
 * @param realloc Memory re-allocation function (optional)
 * @param freeme Memory releasing function (optional)
 * @param pb_alloc Memory allocator function for protocol buffers (optional)
 * @param pb_free Memory releasing function for protocol buffer (optional)
 * @returns Spanking new `riak_context` struct
 */
riak_context *riak_context_new(riak_event_base *base,
                               riak_bufferevent *bev,
                               riak_alloc_fn alloc,
                               riak_realloc_fn realloc,
                               riak_free_fn freeme,
                               riak_pb_alloc_fn pb_alloc,
                               riak_pb_free_fn pb_free);

#define riak_context_new_default(base,bev) riak_context_new((base),(bev),NULL,NULL,NULL,NULL,NULL)

#endif /* RIAK_CONTEXT_H_ */
