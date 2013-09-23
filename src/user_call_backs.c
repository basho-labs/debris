/*********************************************************************
 *
 * call_backs.c: Riak C Message Test Callbacks
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
#include <ctype.h>
#include <unistd.h>
#include <getopt.h>
#include <stdint.h>
#include <event2/event.h>
#include <event2/bufferevent.h>

#include "riak.h"
#include "riak_pb_message.h"
#include "riak_utils.h"
#include "riak_binary.h"
#include "riak.pb-c.h"
#include "riak_kv.pb-c.h"

void ping_cb(riak_ping_response *response, void *ptr) {
    riak_event   *rev = (riak_event*)ptr;
    riak_log(rev, RIAK_LOG_DEBUG, "ping_cb");
    riak_log(rev, RIAK_LOG_DEBUG, "PONG");
}

void listbucket_cb(riak_listbuckets_response *response, void *ptr) {
    riak_event   *rev = (riak_event*)ptr;
    riak_log(rev, RIAK_LOG_DEBUG, "listbucket_cb");
    riak_log(rev, RIAK_LOG_DEBUG, "n_buckets = %d", response->n_buckets);
    int i;
    char name[1024];
    for(i = 0; i < response->n_buckets; i++) {
        riak_binary_dump_ptr(response->buckets[i], name, 1024);
        riak_log(rev, RIAK_LOG_DEBUG, "%d - %s", i, name);
    }
    riak_log(rev, RIAK_LOG_DEBUG, "done = %d", response->done);
}

void listkey_cb(riak_listkeys_response *response, void *ptr) {
    riak_event   *rev = (riak_event*)ptr;
    riak_log(rev, RIAK_LOG_DEBUG, "listkey_cb");
    riak_log(rev, RIAK_LOG_DEBUG, "n_keys = %d", response->n_keys);
    int i;
    char name[1024];
    for(i = 0; i < response->n_keys; i++) {
        riak_binary_dump_ptr(response->keys[i], name, 1024);
        riak_log(rev, RIAK_LOG_DEBUG, "%d - %s", i, name);
    }
    riak_log(rev, RIAK_LOG_DEBUG, "done = %d", response->done);
}

void get_cb(riak_get_response *response, void *ptr) {
    riak_event   *rev = (riak_event*)ptr;
    riak_log(rev, RIAK_LOG_DEBUG, "get_cb");
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
        wrote = riak_object_dump_ptr(response->content[i], target, len);
    }
    riak_log(rev, RIAK_LOG_DEBUG, "%s\n", output);
}

void put_cb(riak_put_response *response, void *ptr) {
    riak_event   *rev = (riak_event*)ptr;
    riak_log(rev, RIAK_LOG_DEBUG, "put_cb\n");
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
        wrote = riak_object_dump_ptr(response->content[i], target, len);
    }
    riak_log(rev, RIAK_LOG_DEBUG, "%s\n", output);
}

void delete_cb(riak_delete_response *response, void *ptr) {
    riak_event   *rev = (riak_event*)ptr;
    riak_log(rev, RIAK_LOG_DEBUG, "delete_cb");
}
