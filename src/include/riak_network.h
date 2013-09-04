/*********************************************************************
 *
 * riak_network.h: Riak C General Networking Utilities
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

#ifndef RIAK_NETWORK_H_
#define RIAK_NETWORK_H_

riak_error resolve_address(riak_context *ctx,
                           const char *host,
                           const char *portnum,
                           riak_addrinfo **addrinfo);

riak_socket_t just_open_a_socket(riak_context *ctx,
                                 riak_addrinfo *addrinfo);

//int just_open_a_socket(riak_context *ctx,
//                       riak_bufferevent *bev,
//                       riak_addrinfo *addrinfo);

#endif
