/*********************************************************************
 *
 * riak_network.c: Riak C General Networking Utilities
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

#include <errno.h>
#include "riak.h"

riak_error resolve_address(riak_context *ctx,
                           const char *host,
                           const char *portnum,
                           riak_addrinfo **addrinfo) {
    riak_addrinfo addrhints;

    // Build the hints to tell getaddrinfo how to act.
    bzero(&addrhints, sizeof(riak_addrinfo));
//   addrhints.ai_family   = AF_UNSPEC; // IPv6 seems a bit broken for now
    addrhints.ai_family   = AF_INET; // IPv4 works like a champ
    addrhints.ai_socktype = SOCK_STREAM;
    addrhints.ai_protocol = IPPROTO_TCP; // We want a TCP socket
    /* Only return addresses we can use. */
    addrhints.ai_flags = EVUTIL_AI_ADDRCONFIG;

    // Use nice, platform agnostic DNS lookup and return an array of results
    int err = evutil_getaddrinfo(host, portnum, &addrhints, addrinfo);
    if (err != 0) {
        riak_log(ctx, RIAK_LOG_FATAL, "Error while resolving '%s:%s': %s\n",
                 host, portnum, evutil_gai_strerror(err));
        return ERIAK_DNS_RESOLUTION;
    }

    return ERIAK_OK;
}

void print_host(riak_addrinfo *addrinfo, char* target, int len, riak_uint16_t *port) {
    struct sockaddr_in  *ipv4;
    struct sockaddr_in6 *ipv6;
    switch (addrinfo->ai_addr->sa_family) {
    case AF_INET:
        ipv4 = (struct sockaddr_in*)addrinfo->ai_addr;
        evutil_inet_ntop(ipv4->sin_family, &(ipv4->sin_addr), target, len);
        *port = ntohs(ipv4->sin_port);
        break;
    case AF_INET6:
        ipv6 = (struct sockaddr_in6*)addrinfo->ai_addr;
        evutil_inet_ntop(ipv6->sin6_family, &(ipv6->sin6_addr), target, len);
        *port = ntohs(ipv6->sin6_port);
        break;
    default:
        strlcpy(target, "<Unknown>", len);
        port = 0;
    }
}

riak_socket_t just_open_a_socket(riak_context  *ctx,
                                 riak_addrinfo *addrinfo) {

//    int
//    bufferevent_socket_connect(struct bufferevent *bev,
//        struct sockaddr *sa, int socklen)

    riak_socket_t sock = socket(addrinfo->ai_family,
                                addrinfo->ai_socktype,
                                addrinfo->ai_protocol);
    if (sock < 0) {
        riak_log(ctx, RIAK_LOG_FATAL, "Could not open a socket\n");
        return -1;
    }
#if 0
    if (evutil_make_socket_nonblocking(sock) < 0) {
        EVUTIL_CLOSESOCKET(sock);
        riak_log(ctx, RIAK_LOG_FATAL, "Could make socket non-blocking\n");
        return -1;
    }
#endif
    int err = connect(sock, addrinfo->ai_addr, addrinfo->ai_addrlen);
    if (err) {
        // Since this is nonblocking, we'll need to treat some errors
        // (like EINTR and EAGAIN) specially.
        if (errno != EINPROGRESS && errno != EAGAIN) {
            char ip[INET6_ADDRSTRLEN];
            riak_uint16_t port;
            print_host(addrinfo, ip, sizeof(ip), &port);
            EVUTIL_CLOSESOCKET(sock);
            riak_log(ctx, RIAK_LOG_FATAL, "Could not connect a socket to host %s:%d [%s]\n", ip, port, strerror(errno));
            return -1;
        }
    }
    return sock;
}


