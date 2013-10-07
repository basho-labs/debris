/*********************************************************************
 *
 * riak_error.c: Riak C Error Handling
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

#define _RIAK_ERROR_DEFINE_MSGS
#include "riak_error.h"


const char* riak_strerror(riak_error err) {
    if (err >= ERIAK_OK && err < ERIAK_LAST_ERRORNUM) {
        return 0;
    }
    return "<Unknown Error>";
}
