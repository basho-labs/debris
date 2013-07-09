/*********************************************************************
 *
 * riak_types.h: Riak C Client Types
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

#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <event2/event.h>

#include "riak.h"
#include "riak_utils.h"
#include "riak.pb-c.h"
#include "riak_kv.pb-c.h"

int main (int argc, char *argv[])
{
    if (argc != 3) {
        printf("Trivial PBC Riak C client\n"
               "Syntax: %s [hostname] [port]\n"
               "Example: %s localhost 10017\n",argv[0],argv[0]);
        return 1;
    }

    event_enable_debug_mode();
    struct event_base *base = event_base_new();
    struct evdns_base *dns_base = evdns_base_new(base, 1);

//    bev = bufferevent_socket_new(base, -1, BEV_OPT_CLOSE_ON_FREE|BEV_OPT_DEFER_CALLBACKS);
    struct bufferevent *bev = bufferevent_socket_new(base, -1, BEV_OPT_CLOSE_ON_FREE);
    riak_context *ctx = riak_context_new_default(base, bev);
    bufferevent_setcb(bev, riak_list_buckets_callback, write_callback, eventcb, ctx);
    bufferevent_enable(bev, EV_READ|EV_WRITE);

    riak_encode_list_buckets_request(ctx);

    bufferevent_socket_connect_hostname(bev, dns_base, AF_UNSPEC, argv[1], atoi(argv[2]));
    event_base_dispatch(base);

    return 0;
}
