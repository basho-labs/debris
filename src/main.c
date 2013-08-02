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
#include "riak_pb_message.h"

void listbucket_cb(riak_listbuckets_response *response, void *ptr) {
    fprintf(stderr, "listbucket_cb\n");
    fprintf(stderr, "n_buckets = %d\n", response->n_buckets);
    int i;
    char name[1024];
    for(i = 0; i < response->n_buckets; i++) {
        riak_binary_dump(response->buckets[i], name, 1024);
        fprintf(stderr, "%d - %s\n", i, name);
     }
    fprintf(stderr, "done = %d\n", response->done);
}

void get_cb(riak_get_response *response, void *ptr) {
    fprintf(stderr, "get_cb\n");
    char output[10240];
    char buffer[1024];
    int len = 10240;
    riak_binary_hex_dump(response->vclock, buffer, 1024);
    char *target = output;
    int wrote = snprintf(target, len, "V-Clock: %s\n", buffer);
    len -= wrote;
    target += wrote;
    wrote = snprintf(target, len, "Unmodified: %s\n", (response->unmodified) ? "true" : "false");
    len -= wrote;
    target += wrote;
    wrote = snprintf(target, len, "Deleted: %s\n", (response->deleted) ? "true" : "false");
    len -= wrote;
    target += wrote;
    wrote = snprintf(target, len, "Objects: %d\n", response->n_content);
    len -= wrote;
    target += wrote;
    riak_uint32_t i;
    for(i = 0; i < response->n_content; i++) {
        wrote = riak_object_dump(response->content[i], target, len);
    }
    fprintf(stderr, "%s\n", output);
}

void put_cb(riak_put_response *response, void *ptr) {
    fprintf(stderr, "put_cb\n");
    char output[10240];
    char buffer[1024];
    int len = 10240;
    char *target = output;
    int wrote;
    if (response->has_vclock) {
        riak_binary_hex_dump(response->vclock, buffer, 1024);
        wrote = snprintf(target, len, "V-Clock: %s\n", buffer);
        len -= wrote;
        target += wrote;
    }
    if (response->has_key) {
        riak_binary_dump(response->key, buffer, 1024);
        wrote = snprintf(target, len, "Key: %s\n", buffer);
        len -= wrote;
        target += wrote;
    }
    wrote = snprintf(target, len, "Objects: %d\n", response->n_content);
    len -= wrote;
    target += wrote;
    riak_uint32_t i;
    for(i = 0; i < response->n_content; i++) {
        wrote = riak_object_dump(response->content[i], target, len);
    }
    fprintf(stderr, "%s\n", output);
}

int main (int argc, char *argv[])
{
    if (argc != 5) {
        printf("Trivial PBC Riak C client\n"
               "Syntax: %s [hostname] [port] [bucket] [key]\n"
               "Example: %s localhost 10017 baz 12345\n",argv[0],argv[0]);
        return 1;
    }

    event_enable_debug_mode();
    struct event_base *base = event_base_new();
    struct evdns_base *dns_base = evdns_base_new(base, 1);

//    bev = bufferevent_socket_new(base, -1, BEV_OPT_CLOSE_ON_FREE|BEV_OPT_DEFER_CALLBACKS);
    struct bufferevent *bev = bufferevent_socket_new(base, -1, BEV_OPT_CLOSE_ON_FREE);
    riak_context *ctx = riak_context_new_default();
    bufferevent_enable(bev, EV_READ|EV_WRITE);

#ifdef LISTBUCKETS
    riak_response_callback cb = (riak_response_callback)listbucket_cb;
    riak_event *listbuckets_ev = riak_event_new(ctx, base, bev, cb, NULL);
    bufferevent_setcb(bev, riak_read_result_callback, write_callback, eventcb, listbuckets_ev);
    riak_encode_listbuckets_request(listbuckets_ev);
#elif GETVALUE
    riak_response_callback cb = (riak_response_callback)get_cb;
    riak_event *get_ev = riak_event_new(ctx, base, bev, cb, NULL);
    bufferevent_setcb(bev, riak_read_result_callback, write_callback, eventcb, get_ev);
    riak_encode_get_request(get_ev,
            argv[3],
            argv[4],
            NULL);
#else
    riak_response_callback cb = (riak_response_callback)put_cb;
    riak_event *put_ev = riak_event_new(ctx, base, bev, cb, NULL);
    bufferevent_setcb(bev, riak_read_result_callback, write_callback, eventcb, put_ev);
    riak_object *obj = riak_object_new(ctx);
    if (obj == NULL) {
        return 1;
    }
    obj->bucket.data = (riak_uint8_t*)argv[3]; // Not copied
    obj->bucket.len = strlen(argv[3]);
    obj->value.data = (riak_uint8_t*)argv[4];
    obj->value.len = strlen(argv[4]);
    riak_encode_put_request(put_ev, obj, NULL);
#endif

    bufferevent_socket_connect_hostname(bev, dns_base, AF_UNSPEC, argv[1], atoi(argv[2]));
    event_base_dispatch(base);

    return 0;
}
