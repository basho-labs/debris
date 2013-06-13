/*
 * -------------------------------------------------------------------
 * riak-c-client
 *
 * Copyright (c) 2013 Dave Parfitt
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
 * -------------------------------------------------------------------
 */


#ifndef RIAK_UTILS_H
#define RIAK_UTILS_H
#include "rcc.h"

#define riak_free(p) _riak_free((void**)&(p))
void _riak_free(void **p);
// helper fn's
struct riak_response *new_riak_response();
void free_riak_response(struct riak_response*);

struct riak_object *new_riak_object();

void free_riak_object(struct riak_object*);

// TODO: NOT CHARSET SAFE, need iconv
struct riak_binary *new_riak_binary(size_t len, char *data);
void populate_riak_binary(struct riak_binary *b, size_t len, uint8_t *data);
void free_riak_binary(struct riak_binary*);



#endif
