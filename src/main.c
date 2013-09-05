/*********************************************************************
 *
 * riak_types.h: Riak C Main
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
#include <event2/event.h>

#include "riak.h"
#include "riak_utils.h"
#include "riak_error.h"
#include "riak.pb-c.h"
#include "riak_kv.pb-c.h"
#include "riak_pb_message.h"
#include "riak_network.h"
#include "call_backs.h"

void usage(FILE *fp, char *progname) {
    fprintf(fp, "Usage:\n");
    fprintf(fp, "%s "
            "[--ping|--get|--put|--list-buckets|--delete|--set-clident|--get-clident|\n"
            " --server-info|--list-keys|--get-bucket|--set-bucket|--map-reduce|--index|\n"
            " --search] [--bucket <name>] [--key <name>] [--value <name>]\n"
            "[--host <localhost>] [--port 10017] [--iterate <n>] [--timeout <secs>]\n", progname);
    exit(1);
}

int main(int argc, char *argv[])
{
    static int operation;
    char bucket[1024];
    char host[256];
    char portnum[6];
    char key[1024];
    char value[1024];
    riak_int32_t   iterate    = 1;
    riak_int32_t   port       = 10017;
    riak_int32_t   timeout    = 10;
    riak_boolean_t has_bucket = RIAK_FALSE;
    riak_boolean_t has_key    = RIAK_FALSE;
    riak_boolean_t has_value  = RIAK_FALSE;
    int c;

    strcpy(host, "localhost");
    strcpy(portnum, "10017");

    while (1) {
        static struct option long_options[] = {
            /* These options set a flag. */
            {"get",          no_argument, &operation, MSG_RPBGETREQ},
            {"put",          no_argument, &operation, MSG_RPBPUTREQ},
            {"list-buckets", no_argument, &operation, MSG_RPBLISTBUCKETSREQ},
            {"ping",         no_argument, &operation, MSG_RPBPINGREQ},
            {"get-clident",  no_argument, &operation, MSG_RPBGETCLIENTIDREQ},
            {"set-clident",  no_argument, &operation, MSG_RPBSETCLIENTIDREQ},
            {"server-info",  no_argument, &operation, MSG_RPBGETSERVERINFOREQ},
            {"delete",       no_argument, &operation, MSG_RPBDELREQ},
            {"list-keys",    no_argument, &operation, MSG_RPBLISTKEYSREQ},
            {"get-bucket",   no_argument, &operation, MSG_RPBGETBUCKETREQ},
            {"set-bucket",   no_argument, &operation, MSG_RPBSETBUCKETREQ},
            {"map-reduce",   no_argument, &operation, MSG_RPBMAPREDREQ},
            {"index",        no_argument, &operation, MSG_RPBINDEXRESP},
            {"search",       no_argument, &operation, MSG_RPBSEARCHQUERYREQ},

            /* These options don't set a flag.
            We distinguish them by their indices. */
            {"bucket",       required_argument, NULL, 'b'},
            {"host",         required_argument, NULL, 'h'},
            {"iterate",      required_argument, NULL, 'i'},
            {"key",          required_argument, NULL, 'k'},
            {"port",         required_argument, NULL, 'p'},
            {"timeout",      required_argument, NULL, 't'},
            {"value",        required_argument, NULL, 'v'},
            {NULL, 0, NULL, 0}
        };
        /* getopt_long stores the option index here. */
        int option_index = 0;

        c = getopt_long (argc, argv, "b:h:i:k:p:t:v:",
                        long_options, &option_index);

         /* Detect the end of the options. */
        if (c == -1)
            break;

        switch (c) {
            case 0:
                /* If this option set a flag, do nothing else now. */
                if (long_options[option_index].flag != 0) {
                    break;
                }
                printf ("option %s", long_options[option_index].name);
                if (optarg)
                printf (" with arg %s", optarg);
                printf ("\n");
                break;

            case 'b':
                printf ("option -b with value `%s'\n", optarg);
                strlcpy(bucket, optarg, 1024);
                has_bucket = RIAK_TRUE;
                break;

            case 'h':
                printf ("option -h with value `%s'\n", optarg);
                strlcpy(host, optarg, sizeof(host));
                break;

            case 'i':
                printf ("option -i with value `%s'\n", optarg);
                iterate = atol(optarg);
                break;

            case 'k':
                printf ("option -k with value `%s'\n", optarg);
                strlcpy(key, optarg, sizeof(key));
                has_key = RIAK_TRUE;
                break;

            case 'p':
                printf ("option -p with value `%s'\n", optarg);
                strlcpy(portnum, optarg, sizeof(portnum));
                port = atol(optarg);
                break;

            case 't':
                printf ("option -t with value `%s'\n", optarg);
                timeout = atol(optarg);
                break;

            case 'v':
                printf ("option -v with value `%s'\n", optarg);
                strlcpy(value, optarg, sizeof(value));
                has_value = RIAK_TRUE;
                break;

            case '?':
                /* getopt_long already printed an error message. */
                usage(stderr, argv[0]);
                break;

            default:
                abort();
        }
    }
    // These options require a bucket
    switch (operation) {
    case MSG_RPBGETREQ:
    case MSG_RPBPUTREQ:
    case MSG_RPBDELREQ:
    case MSG_RPBLISTKEYSREQ:
    case MSG_RPBGETBUCKETREQ:
    case MSG_RPBSETBUCKETREQ:
        if (!has_bucket) {
            fprintf(stderr, "--bucket parameter required\n");
            return 1;
        }
    }

    // These operations require a key
    switch (operation) {
    case MSG_RPBGETREQ:
    case MSG_RPBDELREQ:
        if (!has_key ) {
            fprintf(stderr, "--key parameter required\n");
            return 1;
        }
    }

    // These operations require a value
    switch (operation) {
    case MSG_RPBPUTREQ:
    case MSG_RPBSETCLIENTIDREQ:
    case MSG_RPBSEARCHQUERYREQ:
        if (!has_value) {
            fprintf(stderr, "--value parameter required\n");
            return 1;
        }
    }

/*
    {"get-clident",  no_argument, &operation, MSG_RPBGETCLIENTIDREQ},
    {"set-clident",  no_argument, &operation, MSG_RPBSETCLIENTIDREQ},
    {"map-reduce",   no_argument, &operation, MSG_RPBMAPREDREQ},
    {"index",        no_argument, &operation, MSG_RPBINDEXRESP},
    */

    event_enable_debug_mode();
    evthread_enable_lock_debugging();
//    event_use_pthreads();
//    event_enable_debug_logging(EVENT_DBG_ALL);
    struct event_base *base = event_base_new();
//    struct evdns_base *dns_base = evdns_base_new(base, 1);

    riak_context *ctx = riak_context_new_default();
    riak_response_callback cb;
    riak_event *rev;
    riak_object *obj;
    int it;

    riak_addrinfo *addrinfo;
    riak_error err = riak_resolve_address(ctx, host, portnum, &addrinfo);
    if (err) exit(1);

    // If there was no error, we should have at least one answer so use the 1st
    assert(addrinfo);

    riak_socket_t sock = riak_just_open_a_socket(ctx, addrinfo);
    if (sock < 0) exit(1);

    for(it = 0; it < iterate; it++) {
        riak_log(ctx, RIAK_LOG_DEBUG, "Loop %d\n", it);
//        struct bufferevent *bev = bufferevent_socket_new(base, sock, BEV_OPT_CLOSE_ON_FREE|BEV_OPT_DEFER_CALLBACKS|BEV_OPT_THREADSAFE);
        struct bufferevent *bev = bufferevent_socket_new(base, sock, BEV_OPT_CLOSE_ON_FREE|BEV_OPT_DEFER_CALLBACKS);
        int enabled = bufferevent_enable(bev, EV_READ|EV_WRITE);
        if (enabled != 0) {
            riak_log(ctx, RIAK_LOG_FATAL, "Could not enable bufferevent 0x%llx\n", (riak_uint64_t)bev);
            exit(1);
        }
        riak_binary bucket_bin;
        riak_binary key_bin;
        bucket_bin.data = (riak_uint8_t*)bucket; // Not copied
        bucket_bin.len = strlen(bucket);
        key_bin.data = (riak_uint8_t*)key;
        key_bin.len = strlen(key);

        switch (operation) {
        case MSG_RPBPINGREQ:
            cb = (riak_response_callback)ping_cb;
            rev = riak_event_new(ctx, base, bev, cb, NULL);
            bufferevent_setcb(bev, riak_read_result_callback, write_callback, eventcb, rev);
            riak_encode_ping_request(rev);
            break;
        case MSG_RPBGETREQ:
            cb = (riak_response_callback)get_cb;
            rev = riak_event_new(ctx, base, bev, cb, NULL);
            bufferevent_setcb(bev, riak_read_result_callback, write_callback, eventcb, rev);
            riak_encode_get_request(rev, bucket, key, NULL);
            break;
        case MSG_RPBPUTREQ:
            cb = (riak_response_callback)put_cb;
            rev = riak_event_new(ctx, base, bev, cb, NULL);
            bufferevent_setcb(bev, riak_read_result_callback, write_callback, eventcb, rev);
            obj = riak_object_new(ctx);
            if (obj == NULL) {
                return 1;
            }
            obj->bucket.data = (riak_uint8_t*)bucket; // Not copied
            obj->bucket.len = strlen(bucket);
            obj->value.data = (riak_uint8_t*)value;
            obj->value.len = strlen(value);
            riak_encode_put_request(rev, obj, NULL);
            break;
        case MSG_RPBDELREQ:
            cb = (riak_response_callback)delete_cb;
            rev = riak_event_new(ctx, base, bev, cb, NULL);
            bufferevent_setcb(bev, riak_read_result_callback, write_callback, eventcb, rev);
            riak_encode_delete_request(rev, &bucket_bin, &key_bin, NULL);
            break;
        case MSG_RPBLISTBUCKETSREQ:
            cb = (riak_response_callback)listbucket_cb;
            rev = riak_event_new(ctx, base, bev, cb, NULL);
            bufferevent_setcb(bev, riak_read_result_callback, write_callback, eventcb, rev);
            riak_encode_listbuckets_request(rev);
            break;
        case MSG_RPBLISTKEYSREQ:
            cb = (riak_response_callback)listkey_cb;
            rev = riak_event_new(ctx, base, bev, cb, NULL);
            bufferevent_setcb(bev, riak_read_result_callback, write_callback, eventcb, rev);
            riak_encode_listkeys_request(rev, bucket, timeout * 1000);
            break;
        default:
            usage(stderr, argv[0]);
        }

#if UNUSED
        int connected = bufferevent_socket_connect_hostname(bev, dns_base, AF_UNSPEC, host, port);
//        int connected = bufferevent_socket_connect(bev, addrinfo->ai_addr, addrinfo->ai_addrlen);
        if (connected != 0) {
            fprintf(stderr, "Could not connect to %s:%d\n", host, port);
            exit(1);
        }
#endif
    }

    // Done looking up hostnames
//    evdns_base_free(dns_base, 0);

    // What has been queued up
    event_base_dump_events(base, stderr);

    // Terminates only on error or timeout
    event_base_dispatch(base);

    evutil_freeaddrinfo(addrinfo);

    return 0;
}
