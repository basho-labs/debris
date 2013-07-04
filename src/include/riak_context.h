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
// What about simply using ProtobufCAllocator?

typedef     void *(*riak_alloc_fn)(size_t sz);
typedef     void *(*riak_realloc_fn)(void *ptr, size_t size);
typedef     void (*riak_free_fn)(void *ptr);

typedef struct _riak_context {
    riak_alloc_fn     malloc_fn;
    riak_realloc_fn   realloc_fn;
    riak_free_fn      free_fn;
    riak_bufferevent *bevent;
} riak_context;


/**
 * @brief Construct a Riak Context
 * @param alloc Memory allocator function
 * @param realloc Memory re-allocation function
 * @param freeme Memory releasing function
 * @returns New `riak_context` struct
 */
riak_context *riak_context_new(riak_alloc_fn alloc, riak_realloc_fn realloc, riak_free_fn freeme);

void riak_context_set_event(riak_context*, riak_bufferevent*);
#endif /* RIAK_CONTEXT_H_ */
