/*********************************************************************
 *
 * riak_bucket_props.h: Riak C Client Bucket Properties
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

#ifndef RIAK_BUCKET_PROPS_H
#define RIAK_BUCKET_PROPS_H

typedef enum _riak_bucket_repl_mode {
    RIAK_BUCKET_PROPS_REPL_FALSE = 0,
    RIAK_BUCKET_PROPS_REPL_REALTIME = 1,
    RIAK_BUCKET_PROPS_REPL_FULLSYNC = 2,
    RIAK_BUCKET_PROPS_REPL_TRUE = 3
} riak_bucket_repl_mode;

typedef struct _riak_bucket_mod_fun riak_bucket_mod_fun;
typedef struct _riak_bucket_commit_hook riak_bucket_commit_hook;
typedef struct _riak_bucket_props riak_bucket_props;

/**
 * @brief Construct a new Riak Bucket Property object
 * @param ctx Riak Context
 * @return Riak Bucket Property object
 */
riak_bucket_props*
riak_bucket_props_new(riak_context *ctx);

/**
 * @brief Release claimed memory used by a Riak Bucket Property object
 * @param ctx Riak Context
 * @param props Riak Bucket Property object to be freed
 */
void
riak_bucket_props_free(riak_context       *ctx,
                       riak_bucket_props **props);

/**
 * @brief Print contents of a Riak Bucket Property object to a string
 * @param props Bucket Property object to print
 * @param target Location to write formatted string
 * @param len Number of bytes to write
 * @return Number of bytes written
 */
int
riak_bucket_props_print(riak_bucket_props  *props,
                        char               *target,
                        riak_uint32_t       len);

#endif // RIAK_BUCKET_PROPS_H
