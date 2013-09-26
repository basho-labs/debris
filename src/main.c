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
#include "riak_pb_message.h"
#include "riak_utils.h"
#include "riak_error.h"
#include "riak_binary.h"
#include "riak.pb-c.h"
#include "riak_kv.pb-c.h"
#include "riak_network.h"
#include "user_call_backs.h"

void usage(FILE *fp, char *progname) {
    fprintf(fp, "Usage:\n");
    fprintf(fp, "%s "
            "[--ping|--get|--put|--list-buckets|--delete|--set-clident|--get-clident|\n"
            " --server-info|--list-keys|--get-bucket|--set-bucket|--map-reduce|--index|\n"
            " --search] [--bucket <name>] [--key <name>] [--value <name>]\n"
            "[--host <localhost>] [--port 10017] [--iterate <n>] [--timeout <secs>]\n", progname);
    exit(1);
}

typedef struct {
    riak_boolean_t async;
    riak_int32_t   iterate;
    riak_int32_t   port;
    riak_int32_t   timeout;
    riak_boolean_t has_bucket;
    riak_boolean_t has_key;
    riak_boolean_t has_value;
    char bucket[1024];
    char host[256];
    char portnum[6];
    char key[1024];
    char value[1024];
} riak_args;

/**
 * @brief Parse the command-line arguments
 * @param argc Number of arguments
 * @param argv Value of arguments
 * @param args Return value of parsed arguments
 * @return -1 on failure; > 0 on success
 */
int
riak_parse_args(int        argc,
                char      *argv[],
                riak_args *args) {
    static int operation;  // static required for getopt
    int  c;

    memset((void*)args, '\0', sizeof(riak_args));
    args->async      = RIAK_FALSE;
    args->iterate    = 1;
    args->port       = 10017;
    args->timeout    = 10;
    args->has_bucket = RIAK_FALSE;
    args->has_key    = RIAK_FALSE;
    args->has_value  = RIAK_FALSE;

    strcpy(args->host, "localhost");
    strcpy(args->portnum, "10017");

    // Keep parsing args as long as we can
    while (1) {
        static struct option long_options[] = {
            // These options set a flag.
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

            // These options don't set a flag.
            // We distinguish them by their indices.
            {"async",        no_argument,       NULL, 'a'},
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

        c = getopt_long (argc, argv, "ab:h:i:k:p:t:v:",
                        long_options, &option_index);

         // Detect the end of the options.
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

            case 'a':
                args->async = RIAK_TRUE;
                break;

            case 'b':
                printf ("option -b with value `%s'\n", optarg);
                riak_strlcpy(args->bucket, optarg, sizeof(args->bucket));
                args->has_bucket = RIAK_TRUE;
                break;

            case 'h':
                printf ("option -h with value `%s'\n", optarg);
                riak_strlcpy(args->host, optarg, sizeof(args->host));
                break;

            case 'i':
                printf ("option -i with value `%s'\n", optarg);
                args->iterate = atol(optarg);
                break;

            case 'k':
                printf ("option -k with value `%s'\n", optarg);
                riak_strlcpy(args->key, optarg, sizeof(args->key));
                args->has_key = RIAK_TRUE;
                break;

            case 'p':
                printf ("option -p with value `%s'\n", optarg);
                riak_strlcpy(args->portnum, optarg, sizeof(args->portnum));
                args->port = atol(optarg);
                break;

            case 't':
                printf ("option -t with value `%s'\n", optarg);
                args->timeout = atol(optarg);
                break;

            case 'v':
                printf ("option -v with value `%s'\n", optarg);
                riak_strlcpy(args->value, optarg, sizeof(args->value));
                args->has_value = RIAK_TRUE;
                break;

            case '?':
                /* getopt_long already printed an error message. */
                usage(stderr, argv[0]);
                break;

            default:
                return -1;
        }
    }
    return operation;
}

int
main(int   argc,
     char *argv[])
{
    riak_args args;
    int operation = riak_parse_args(argc, argv, &args);

    // These options require a bucket
    switch (operation) {
    case MSG_RPBGETREQ:
    case MSG_RPBPUTREQ:
    case MSG_RPBDELREQ:
    case MSG_RPBLISTKEYSREQ:
    case MSG_RPBGETBUCKETREQ:
    case MSG_RPBSETBUCKETREQ:
        if (!args.has_bucket) {
            fprintf(stderr, "--bucket parameter required\n");
            return 1;
        }
    }

    // These operations require a key
    switch (operation) {
    case MSG_RPBGETREQ:
    case MSG_RPBDELREQ:
        if (!args.has_key ) {
            fprintf(stderr, "--key parameter required\n");
            return 1;
        }
    }

    // These operations require a value
    switch (operation) {
    case MSG_RPBPUTREQ:
    case MSG_RPBSETCLIENTIDREQ:
    case MSG_RPBSEARCHQUERYREQ:
        if (!args.has_value) {
            fprintf(stderr, "--value parameter required\n");
            return 1;
        }
    }

    event_enable_debug_mode();
//    event_use_pthreads();

    riak_context *ctx;
    riak_error err = riak_context_new_default(&ctx, args.host, args.portnum);
    if (err) {
        exit(1);
    }
    riak_event  *rev;
    riak_object *obj;
    riak_put_options put_options;
    int it;

    for(it = 0; it < args.iterate; it++) {
        riak_log_context(ctx, RIAK_LOG_DEBUG, "Loop %d", it);

        riak_binary bucket_bin;
        riak_binary key_bin;
        riak_binary_from_string(bucket_bin, args.bucket); // Not copied
        riak_binary_from_string(key_bin, args.key); // Not copied

        if (args.async) {
            rev = riak_event_new(ctx, NULL, NULL, NULL, NULL);
            if (rev == NULL) {
                return 1;
            }
            // For convenience have user callback know about its riak_event
            riak_event_set_cb_data(rev, rev);
        }
        switch (operation) {
        case MSG_RPBPINGREQ:
            if (args.async) {
                riak_event_set_response_cb(rev, (riak_response_callback)ping_cb);
                riak_encode_ping_request(rev, &(rev->request));
            } else {
                err = riak_ping(ctx);
                if (err) {
                    fprintf(stderr, "No Ping\n");
                }
            }
            break;
        case MSG_RPBGETREQ:
            if (args.async) {
                riak_event_set_response_cb(rev, (riak_response_callback)get_cb);
                riak_encode_get_request(rev, &bucket_bin, &key_bin, NULL, &(rev->request));
            } else {
                char output[10240];
                riak_get_response *get_response;
                err = riak_get(ctx, &bucket_bin, &key_bin, NULL, &get_response);
                if (err) {
                    fprintf(stderr, "Get Problems\n");
                }
                riak_print_get_response(get_response, output, sizeof(output));
                printf("%s\n", output);
                riak_free_get_response(ctx, &get_response);
            }
            break;
        case MSG_RPBPUTREQ:
            obj = riak_object_new(ctx);
            if (obj == NULL) {
                riak_log(rev, RIAK_LOG_FATAL, "Could not allocate a Riak Object");
                return 1;
            }
            riak_binary_from_string(obj->bucket, args.bucket); // Not copied
            riak_binary_from_string(obj->value, args.value); // Not copied
            memset(&put_options, '\0', sizeof(riak_put_options));
            put_options.has_return_head = RIAK_TRUE;
            put_options.return_head = RIAK_TRUE;
            put_options.has_return_body = RIAK_TRUE;
            put_options.return_body = RIAK_TRUE;
            if (args.async) {
                riak_event_set_response_cb(rev, (riak_response_callback)put_cb);
                riak_encode_put_request(rev, obj, &put_options, &(rev->request));
            } else {
                riak_put_response *put_response;
                char output[10240];
                err = riak_put(ctx, obj, &put_options, &put_response);
                if (err) {
                    fprintf(stderr, "Put Problems\n");
                }
                riak_print_put_response(put_response, output, sizeof(output));
                printf("%s\n", output);
                riak_free_put_response(ctx, &put_response);
            }
            break;
        case MSG_RPBDELREQ:
            if (args.async) {
                riak_event_set_response_cb(rev, (riak_response_callback)delete_cb);
                riak_encode_delete_request(rev, &bucket_bin, &key_bin, NULL, &(rev->request));
            } else {
                err = riak_delete(ctx, &bucket_bin, &key_bin, NULL);
                if (err) {
                    fprintf(stderr, "Delete Problems\n");
                }
            }
            break;
        case MSG_RPBLISTBUCKETSREQ:
            riak_event_set_response_cb(rev, (riak_response_callback)listbucket_cb);
            riak_encode_listbuckets_request(rev, &(rev->request));
            break;
        case MSG_RPBLISTKEYSREQ:
            riak_event_set_response_cb(rev, (riak_response_callback)listkey_cb);
            riak_encode_listkeys_request(rev, &bucket_bin, args.timeout * 1000, &(rev->request));
            break;
        default:
            usage(stderr, argv[0]);
        }

        if (args.async) {
            err = riak_send_req(rev, rev->request);
            if (err) {
                riak_log(rev, RIAK_LOG_FATAL, "Could not send request");
                exit(1);
            }
        }
    }
    // What has been queued up
    fflush(stdout);

    if (args.async) {
        // Terminates only on error or timeout
        event_base_dispatch(riak_context_get_base(ctx));
    }

    riak_context_free(&ctx);

    return 0;
}
