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
/*
   struct riak_binary *bucket = new_riak_binary(3, "Foo");
   struct riak_binary *key = new_riak_binary(3, "Bar");
   struct riak_response* response = new_riak_response();
   struct riak_get_options opts;

   struct riak_protocol pb = setup_riak_pb_proto();
   struct riak_context ctx;
   ctx.proto = &pb;
   ctx.malloc_fn=malloc;
   riak_get(&ctx,
             bucket,
             key,
             &opts,
             foo);

   free_riak_binary(bucket);
   free_riak_binary(key);
   free_riak_response(response);
   // typedef void (*event_callback_fn)(evutil_socket_t, short, void *);
   //struct event_base *eb = event_base_new();
   //event_base_free(eb);
   */

    struct event_base *base;
    struct evdns_base *dns_base;
    struct bufferevent *bev;

    if (argc != 3) {
        printf("Trivial PBC Riak C client\n"
               "Syntax: %s [hostname] [port]\n"
               "Example: %s localhost 10017\n",argv[0],argv[0]);
        return 1;
    }

    event_enable_debug_mode();
    base = event_base_new();
    dns_base = evdns_base_new(base, 1);

//    bev = bufferevent_socket_new(base, -1, BEV_OPT_CLOSE_ON_FREE|BEV_OPT_DEFER_CALLBACKS);
    bev = bufferevent_socket_new(base, -1, BEV_OPT_CLOSE_ON_FREE);
    bufferevent_setcb(bev, riak_list_buckets_callback, write_callback, eventcb, base);
    bufferevent_enable(bev, EV_READ|EV_WRITE);

    riak_context *ctx = riak_context_new(NULL, NULL, NULL);
    riak_context_set_event(ctx, bev);
    riak_list_buckets(ctx);

    bufferevent_socket_connect_hostname(bev, dns_base, AF_UNSPEC, argv[1], atoi(argv[2]));
    event_base_dispatch(base);

    return 0;
}
