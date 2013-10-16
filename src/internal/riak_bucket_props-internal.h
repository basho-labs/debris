/*********************************************************************
 *
 * riak_bucket_props-internal.h: Riak C Client Bucket Properties
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

#include "riak_binary-internal.h"

#ifndef RIAK_INTERNAL_BUCKET_PROPS_H
#define RIAK_INTERNAL_BUCKET_PROPS_H

// Based on RpbModFun
struct _riak_bucket_mod_fun
{
    riak_binary *module;
    riak_binary *function;
};

// Based on RpbCommitHook
struct _riak_bucket_commit_hook
{
    riak_bucket_mod_fun *modfun;
    riak_boolean_t       has_name;
    riak_binary         *name;
};

// Based on RpbBucketProps
struct _riak_bucket_props
{
    riak_boolean_t            has_n_val;
    riak_uint32_t             n_val;
    riak_boolean_t            has_allow_mult;
    riak_boolean_t            allow_mult;
    riak_boolean_t            has_last_write_wins;
    riak_boolean_t            last_write_wins;
    riak_size_t               n_precommit;
    riak_bucket_commit_hook **precommit;
    riak_boolean_t            has_has_precommit;
    riak_boolean_t            has_precommit;
    riak_size_t               n_postcommit;
    riak_bucket_commit_hook **postcommit;
    riak_boolean_t            has_has_postcommit;
    riak_boolean_t            has_postcommit;
    riak_bucket_mod_fun      *chash_keyfun;
    riak_bucket_mod_fun      *linkfun;
    riak_boolean_t            has_old_vclock;
    riak_uint32_t             old_vclock;
    riak_boolean_t            has_young_vclock;
    riak_uint32_t             young_vclock;
    riak_boolean_t            has_big_vclock;
    riak_uint32_t             big_vclock;
    riak_boolean_t            has_small_vclock;
    riak_uint32_t             small_vclock;
    riak_boolean_t            has_pr;
    riak_uint32_t             pr;
    riak_boolean_t            has_r;
    riak_uint32_t             r;
    riak_boolean_t            has_w;
    riak_uint32_t             w;
    riak_boolean_t            has_pw;
    riak_uint32_t             pw;
    riak_boolean_t            has_dw;
    riak_uint32_t             dw;
    riak_boolean_t            has_rw;
    riak_uint32_t             rw;
    riak_boolean_t            has_basic_quorum;
    riak_boolean_t            basic_quorum;
    riak_boolean_t            has_notfound_ok;
    riak_boolean_t            notfound_ok;
    riak_boolean_t            has_backend;
    riak_binary              *backend;
    riak_boolean_t            has_search;
    riak_boolean_t            search;
    riak_boolean_t            has_repl;
    riak_bucket_repl_mode     repl;
    riak_boolean_t            has_yz_index;
    riak_binary               yz_index;
};


#endif //RIAK_INTERNAL_BUCKET_PROPS_H
