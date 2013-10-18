/*********************************************************************
 *
 * riak_print.h: Riak C Client Printing Utilities
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

#ifndef RIAK_PRINT_INTERNAL_H
#define RIAK_PRINT_INTERNAL_H

/**
 * @brief Print an integer value to a string
 * @param name Printable name of filed
 * @param value Value to print
 * @param target Location to write the formatted output (out)
 * @param len Remaining space left in `target` (out)
 * @param total Running total of all bytes written (out)
 * @returns Number of bytes written in current call
 * @note `len` is reduced each time and `total` and `target` increase
 */
riak_int32_t
riak_print_int(char         *name,
               riak_int32_t  value,
               char        **target,
               riak_int32_t *len,
               riak_int32_t *total);
riak_int32_t
riak_print_bool(char           *name,
                riak_boolean_t  value,
                char          **target,
                riak_int32_t   *len,
                riak_int32_t   *total);

riak_int32_t
riak_print_binary(char         *name,
                  riak_binary  *value,
                  char        **target,
                  riak_int32_t *len,
                  riak_int32_t *total);

riak_int32_t
riak_print_time(char         *name,
                riak_int32_t  value,
                char        **target,
                riak_int32_t *len,
                riak_int32_t *total);

riak_int32_t
riak_print_string(char         *value,
                  char        **target,
                  riak_int32_t *len,
                  riak_int32_t *total);

#endif
