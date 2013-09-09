/*********************************************************************
 *
 * riak_types.h: Riak Operations
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

#include "riak.h"
#include "riak_pb_message.h"
#include "riak_kv.pb-c.h"
#include "riak_utils.h"
#include <event2/bufferevent_struct.h>

//int riak_get(riak_context *ctx,
//             riak_binary *bucket,
//             riak_binary *key,
//             riak_get_options *get_options,
//             riak_response_callback resonse_cb) {
//  return 0;
//}

void write_callback(riak_bufferevent *bev, void *ptr)
{
    fprintf(stderr, "Ready for write with event %p.\n", (void*)&(bev->ev_write));
}

