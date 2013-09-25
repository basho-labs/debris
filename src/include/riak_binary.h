/*********************************************************************
 *
 * riak_binary.h: Riak C Client Binary Object Utilities
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


#ifndef RIAK_BINARY_H_
#define RIAK_BINARY_H_

/**
 * @brief Allocate a new `riak_binary` struct
 * @param len Length of binary in bytes
 * @param data Pointer to binary data
 * @returns pointer to newly created `riak_binary` struct
 */
riak_binary *riak_binary_new(riak_context *ctx, riak_size_t len, riak_uint8_t *data);

/**
 * @brief Allocate a new riak_binary and populate from data pointer
 * @param bin Existing `riak_binary` to be populated
 * @param len Length of binary in bytes
 * @param data Source of binary to be copied to bin
 */
void riak_binary_populate(riak_context *ctx, riak_binary *bin, riak_size_t len, riak_uint8_t *data);

/**
 * @brief Free allocated memory used by `riak_binary`
 */
void riak_binary_free(riak_context *ctx, riak_binary *bin);
void riak_binary_copy_ptr(riak_binary* to, riak_binary* from);
#define riak_binary_copy(A,B) riak_binary_copy_ptr(&(A),&(B))
riak_error riak_binary_deep_copy(riak_context *ctx, riak_binary *to, riak_binary *from);
void riak_binary_to_pb_copy_ptr(ProtobufCBinaryData* to, riak_binary* from);
#define riak_binary_to_pb_copy(A,B) riak_binary_to_pb_copy_ptr(&(A),&(B))
riak_error riak_binary_to_pb_deep_copy(riak_context *ctx, ProtobufCBinaryData *to, riak_binary *from);
void riak_binary_from_pb_copy_ptr(riak_binary* to, ProtobufCBinaryData* from);
#define riak_binary_from_pb_copy(A,B) riak_binary_from_pb_copy_ptr(&(A),&(B))
riak_error riak_binary_from_pb_deep_copy_ptr(riak_context *ctx, riak_binary *to, ProtobufCBinaryData *from);
#define riak_binary_from_pb_deep_copy(A,B,C) riak_binary_from_pb_deep_copy_ptr((A),&(B),&(C))
int riak_binary_print_ptr(riak_binary *bin, char* target, riak_uint32_t len);
#define riak_binary_print(A,B,C) riak_binary_print_ptr(&(A),(B),(C))
int riak_binary_hex_print_ptr(riak_binary *bin, char* target, riak_uint32_t len);
#define riak_binary_hex_print(A,B,C) riak_binary_hex_print_ptr(&(A),(B),(C))
void riak_binary_from_string_ptr(riak_binary *to, const char *from);
#define riak_binary_from_string(A,B) riak_binary_from_string_ptr(&(A),B)
riak_error riak_binary_from_string_deep_copy_ptr(riak_context *ctx, riak_binary *to, const char *from);
#define riak_binary_from_string_deep_copy(A,B,C) riak_binary_from_string(A,&(B),C)

#endif /* RIAK_BINARY_H_ */
