/*********************************************************************
 *
 * utils.c: Riak C Client Utilities
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

#include <time.h>
#include "riak.h"
#include "riak_binary-internal.h"
#include "riak_print-internal.h"

riak_int32_t
riak_print_int(char         *name,
               riak_int32_t  value,
               char        **target,
               riak_int32_t *len,
               riak_int32_t *total) {
    riak_int32_t wrote = 0;
    if (*len > 0) {
        wrote = snprintf(*target, *len, "%s: %d\n", name, value);
        *len -= wrote;
        *target += wrote;
        *total += wrote;
    }
    return wrote;
}

riak_int32_t
riak_print_bool(char           *name,
                riak_boolean_t  value,
                char          **target,
                riak_int32_t   *len,
                riak_int32_t   *total) {
    riak_int32_t wrote = 0;
    if (*len > 0) {
        wrote = snprintf(*target, *len, "%s: %s\n",
                         name, (value ? "true" : "false"));
        *len -= wrote;
        *target += wrote;
        *total += wrote;
    }
    return wrote;
}

riak_int32_t
riak_print_binary(char         *name,
                  riak_binary  *value,
                  char        **target,
                  riak_int32_t *len,
                  riak_int32_t *total) {
    char buffer[2048];
    riak_int32_t wrote = 0;
    if (*len > 0) {
        riak_binary_print(value, buffer, sizeof(buffer));
        wrote = snprintf(*target, *len, "%s: %s\n", name, buffer);
        *len -= wrote;
        *target += wrote;
        *total += wrote;
    }
    return wrote;
}

riak_int32_t
riak_print_string(char         *value,
                  char        **target,
                  riak_int32_t *len,
                  riak_int32_t *total) {
    riak_int32_t wrote = 0;
    if (*len > 0) {
        wrote = snprintf(*target, *len, "%s\n", value);
        *len -= wrote;
        *target += wrote;
        *total += wrote;
    }
    return wrote;
}

riak_int32_t
riak_print_time(char         *name,
                riak_int32_t  value,
                char        **target,
                riak_int32_t *len,
                riak_int32_t *total) {
    char buffer[256];
    riak_int32_t wrote = 0;
    if (*len > 0) {
        time_t mod = (time_t)value;
        strftime(buffer, sizeof(buffer), "%Y-%m-%d %H:%M:%S", localtime(&mod));
        wrote = snprintf(*target, *len, "%s: %s\n", name, buffer);
        *len -= wrote;
        *target += wrote;
        *total += wrote;
    }
    return wrote;
}

