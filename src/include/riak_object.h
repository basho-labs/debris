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

typedef struct _riak_link riak_link;
typedef struct _riak_pair riak_pair;
typedef struct _riak_object riak_object;

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
                 riak_object **obj);

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

#endif /* RIAK_OBJECT_H_ */
