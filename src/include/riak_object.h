/*********************************************************************
 *
 * riak_object.h: Riak Object suite
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

#ifndef RIAK_OBJECT_H_
#define RIAK_OBJECT_H_

// Based off of RpbLink
typedef struct _riak_link
{
    riak_boolean_t has_bucket;
    riak_binary    bucket;
    riak_boolean_t has_key;
    riak_binary    key;
    riak_boolean_t has_tag;
    riak_binary    tag;
} riak_link;

// Based off of RpbPair
typedef struct _riak_pair
{
    riak_binary    key;
    riak_boolean_t has_value;
    riak_binary    value;
} riak_pair;

// Based off of RpbContent
typedef struct _riak_object {
    riak_binary bucket;

    riak_boolean_t has_key;
    riak_binary key;

    riak_binary value;

    riak_boolean_t has_charset;
    riak_binary charset;

    riak_boolean_t has_last_mod;
    riak_uint32_t last_mod;

    riak_boolean_t has_last_mod_usecs;
    riak_uint32_t last_mod_usecs;

    riak_boolean_t has_content_type;
    riak_binary content_type;

    riak_boolean_t has_content_encoding;
    riak_binary encoding;

    riak_boolean_t has_deleted;
    riak_boolean_t deleted;

    riak_boolean_t has_vtag;
    riak_binary vtag;

    riak_int32_t n_links;
    riak_link **links;

    riak_int32_t   n_usermeta;
    riak_pair    **usermeta;
    riak_int32_t   n_indexes;
    riak_pair    **indexes;
} riak_object;

/**
 * @brief Construct a new Riak Object
 * @param ctx Riak Context
 * @return Riak Object
 */
riak_object*
riak_object_new(riak_context *ctx);

/**
 * @brief Release claimed memory used by a Riak Object
 * @param ctx Riak Context
 * @param obj Riak Object to be freed
 */
void
riak_object_free(riak_context *ctx,
                 riak_object  *obj);

/**
 * @brief Release claimed memory used by PB Riak Object
 * @param ctx Riak Context
 * @param obj PB Riak Object to be freed
 */
void
riak_object_free_pb(riak_context *ctx,
                    RpbContent   *obj);

/**
 * @brief Print contents of a Riak Object to a string
 * @param obj Object to print
 * @param target Location to write formatted string
 * @param len Number of bytes to write
 * @return Number of bytes written
 */
int
riak_object_print(riak_object  *obj,
                  char         *target,
                  riak_uint32_t len);

/**
 * @brief Copy a Riak Object to a protocol buffer
 * @param ctx Riak Context
 * @param to Protocol buffer target
 * @param from User-supplied `riak_object`
 *
 * @returns Error code
 */
int riak_object_to_pb_copy(riak_context *ctx, RpbContent *to, riak_object *from);

/**
 * @brief Allocate an array of `riak_object` pointers
 * @param ctx Riak Context
 * @param array Returned array of pointers to `riak_object`s
 * @param len Number of elements to allocate
 *
 * @returns Error Code
 */
riak_error
riak_object_new_array(riak_context  *ctx,
                      riak_object ***array,
                      riak_size_t    len);

/**
 * @brief Clean up memory allocated for an array of `riak_objects`
 * @param ctx Riak Context
 * @param array Target of destruction
 * @param len Number of elements to allocate
 */
void
riak_object_free_array(riak_context  *ctx,
                       riak_object ***array,
                       riak_size_t    len);
/**
 * @brief Shallow copy a Riak Object from a protocol buffer
 * @param ctx Riak Context
 * @param to Allocated `riak_object` returned to caller
 * @param from Riak-supplied Protocol buffer source
 *
 * @returns Error code
 */
riak_error
riak_object_new_from_pb(riak_context *ctx, riak_object** to, RpbContent* from);

#endif /* RIAK_OBJECT_H_ */
