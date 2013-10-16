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

#include "riak.h"
#include "riak_messages-internal.h"
#include "riak_command.h"
#include "riak_call_backs-internal.h"
#include "riak_event.h"
#include "riak_event-internal.h"
#include "riak_utils-internal.h"

int
main(int   argc,
     char *argv[])
{
    riak_args args;
    int operation = riak_parse_args(argc, argv, &args);

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
    char output[10240];
    int it;

    riak_binary *bucket_bin = riak_binary_new_from_string(ctx, args.bucket); // Not copied
    riak_binary *key_bin    = riak_binary_new_from_string(ctx, args.key); // Not copied
    riak_binary *value_bin  = riak_binary_new_from_string(ctx, args.value); // Not copied
    if (bucket_bin == NULL ||
        key_bin    == NULL ||
        value_bin  == NULL) {
        fprintf(stderr, "Could not allocate bucket/key/value\n");
        exit(1);
    }

    for(it = 0; it < args.iterate; it++) {
        riak_log_context(ctx, RIAK_LOG_DEBUG, "Loop %d", it);

        if (args.async) {
            rev = riak_event_new(ctx, NULL, NULL, NULL);
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
                riak_encode_ping_request(rev, &(rev->pb_request));
            } else {
                err = riak_ping(ctx);
                if (err) {
                    fprintf(stderr, "No Ping\n");
                }
            }
            break;
        case MSG_RPBGETSERVERINFOREQ:
            if (args.async) {
                riak_event_set_response_cb(rev, (riak_response_callback)serverinfo_cb);
                riak_encode_serverinfo_request(rev, &(rev->pb_request));
            } else {
                riak_serverinfo_response *serverinfo_response;
                err = riak_serverinfo(ctx, &serverinfo_response);
                if (err) {
                    fprintf(stderr, "Server Info Problems\n");
                }
                riak_print_serverinfo_response(serverinfo_response, output, sizeof(output));
                printf("%s\n", output);
                riak_free_serverinfo_response(ctx, &serverinfo_response);
            }
            break;
        case MSG_RPBGETREQ:
            if (args.async) {
                riak_event_set_response_cb(rev, (riak_response_callback)get_cb);
                riak_encode_get_request(rev, bucket_bin, key_bin, NULL, &(rev->pb_request));
            } else {
                riak_get_response *get_response;
                err = riak_get(ctx, bucket_bin, key_bin, NULL, &get_response);
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
            riak_object_set_bucket(obj, riak_binary_new_from_string(ctx, args.bucket)); // Not copied
            riak_object_set_value(obj, riak_binary_new_from_string(ctx, args.value)); // Not copied
            if (riak_object_get_bucket(obj) == NULL ||
                riak_object_get_value(obj) == NULL) {
                fprintf(stderr, "Could not allocate bucket/value\n");
                exit(1);
            }
            memset(&put_options, '\0', sizeof(riak_put_options));
            put_options.has_return_head = RIAK_TRUE;
            put_options.return_head = RIAK_TRUE;
            put_options.has_return_body = RIAK_TRUE;
            put_options.return_body = RIAK_TRUE;
            if (args.async) {
                riak_event_set_response_cb(rev, (riak_response_callback)put_cb);
                riak_encode_put_request(rev, obj, &put_options, &(rev->pb_request));
            } else {
                riak_put_response *put_response;
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
                riak_encode_delete_request(rev, bucket_bin, key_bin, NULL, &(rev->pb_request));
            } else {
                err = riak_delete(ctx, bucket_bin, key_bin, NULL);
                if (err) {
                    fprintf(stderr, "Delete Problems\n");
                }
            }
            break;
        case MSG_RPBLISTBUCKETSREQ:
            if (args.async) {
                riak_event_set_response_cb(rev, (riak_response_callback)listbucket_cb);
                riak_encode_listbuckets_request(rev, &(rev->pb_request));
            } else {
                riak_listbuckets_response *bucket_response;
                err = riak_listbuckets(ctx, &bucket_response);
                if (err) {
                    fprintf(stderr, "List buckets Problems\n");
                }
                riak_print_listbuckets_response(bucket_response, output, sizeof(output));
                riak_log_context(ctx, RIAK_LOG_DEBUG, "%s", output);
                riak_free_listbuckets_response(ctx, &bucket_response);
            }
            break;
        case MSG_RPBLISTKEYSREQ:
            if (args.async) {
                riak_event_set_response_cb(rev, (riak_response_callback)listkey_cb);
                riak_encode_listkeys_request(rev, bucket_bin, args.timeout * 1000, &(rev->pb_request));
            } else {
                riak_listkeys_response *key_response;
                err = riak_listkeys(ctx, bucket_bin, args.timeout * 1000, &key_response);
                if (err) {
                    fprintf(stderr, "List keys Problems\n");
                }
                riak_print_listkeys_response(key_response, output, sizeof(output));
                riak_log_context(ctx, RIAK_LOG_DEBUG, "%s", output);
                riak_free_listkeys_response(ctx, &key_response);
            }
            break;
        case MSG_RPBGETCLIENTIDREQ:
            if (args.async) {
                riak_event_set_response_cb(rev, (riak_response_callback)getclientid_cb);
                riak_encode_get_clientid_request(rev, &(rev->pb_request));
            } else {
                riak_get_clientid_response *getcli_response;
                err = riak_get_clientid(ctx, &getcli_response);
                if (err) {
                    fprintf(stderr, "Get ClientId Problems\n");
                }
                riak_print_get_clientid_response(getcli_response, output, sizeof(output));
                printf("%s\n", output);
                riak_free_get_clientid_response(ctx, &getcli_response);
            }
            break;
        case MSG_RPBSETCLIENTIDREQ:
            if (args.async) {
                riak_event_set_response_cb(rev, (riak_response_callback)setclientid_cb);
                riak_encode_set_clientid_request(rev, value_bin, &(rev->pb_request));
            } else {
                riak_set_clientid_response *setcli_response;
                err = riak_set_clientid(ctx, value_bin, &setcli_response);
                if (err) {
                    fprintf(stderr, "Set ClientId Problems\n");
                }
                riak_free_set_clientid_response(ctx, &setcli_response);
            }
            break;
        default:
            usage(stderr, argv[0]);
        }

        if (args.async) {
            err = riak_send_req(rev, rev->pb_request);
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

    riak_free(ctx, &bucket_bin);
    riak_free(ctx, &key_bin);
    riak_free(ctx, &value_bin);
    riak_context_free(&ctx);

    return 0;
}
