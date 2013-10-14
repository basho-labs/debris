/*********************************************************************
 *
 * riak_object.c: Riak C Object
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
#include "riak_messages-internal.h"
#include "riak_utils-internal.h"
#include "riak_binary-internal.h"
#include "riak_object-internal.h"
#include "riak_context-internal.h"

//
// P A I R S
//
static riak_error
riak_pairs_copy_to_pb(riak_context *ctx,
                      RpbPair    ***pbpair_target,
                      riak_pair   **pair,
                      int           num_pairs) {
    RpbPair **pbpair = (RpbPair**)(ctx->malloc_fn)(sizeof(RpbPair*) * num_pairs);
    if (pbpair == NULL) {
        return ERIAK_OUT_OF_MEMORY;
    }
    int i;
    for(i = 0; i < num_pairs; i++) {
        pbpair[i] = (RpbPair*)(ctx->malloc_fn)(sizeof(RpbPair));
        if (pbpair[i] == NULL) {
            return ERIAK_OUT_OF_MEMORY;
        }
        memset(pbpair[i], '\0', sizeof(RpbPair));
        if (pair[i]->has_value) {
            pbpair[i]->has_value = RIAK_TRUE;
            riak_binary_to_pb_copy(pbpair[i]->value, pair[i]->value);
        }
    }
    // Finally assign the pointer to the list of pair pointers
    *pbpair_target = pbpair;

    return ERIAK_OK;
}

static void
riak_pairs_free_pb(riak_context *ctx,
                   RpbPair    ***pbpair_target,
                   int           num_pairs) {
    RpbPair **pbpair = *pbpair_target;
    int i;
    for(i = 0; i < num_pairs; i++) {
        riak_free(ctx, pbpair[i]);
    }
    riak_free(ctx, pbpair);
    *pbpair_target = NULL;
}

static int
riak_pairs_copy_from_pb(riak_context *ctx,
                        riak_pair  ***pair_target,
                        RpbPair     **pbpair,
                        int           num_pairs) {
    riak_pair **pair = (riak_pair**)(ctx->malloc_fn)(sizeof(riak_pair*) * num_pairs);
    if (pair == NULL) {
        return ERIAK_OUT_OF_MEMORY;
    }
    int i;
    for(i = 0; i < num_pairs; i++) {
        pair[i] = (riak_pair*)(ctx->malloc_fn)(sizeof(riak_pair));
        if (pair[i] == NULL) {
            return ERIAK_OUT_OF_MEMORY;
        }
        memset(pair[i], '\0', sizeof(riak_pair));
        if (pbpair[i]->has_value) {
            pair[i]->has_value = RIAK_TRUE;
            riak_binary_from_pb_copy(pair[i]->value, pbpair[i]->value);
        }
    }
    // Finally assign the pointer to the list of pair pointers
    *pair_target = pair;

    return ERIAK_OK;
}

static void
riak_pairs_free(riak_context *ctx,
                riak_pair  ***pair_target,
                int           num_pairs) {
    riak_pair **pair = *pair_target;
    int i;
    for(i = 0; i < num_pairs; i++) {
        riak_free(ctx, pair[i]);
    }
    riak_free(ctx, pair);
    *pair_target = NULL;
}

//
// L I N K S
//
static riak_error
riak_links_copy_to_pb(riak_context *ctx,
                      RpbLink    ***pblink_target,
                      riak_link   **link,
                      int           num_links) {
    RpbLink **pblink = (RpbLink**)(ctx->malloc_fn)(sizeof(RpbLink*) * num_links);
    if (pblink == NULL) {
        return ERIAK_OUT_OF_MEMORY;
    }
    int i;
    for(i = 0; i < num_links; i++) {
        pblink[i] = (RpbLink*)(ctx->malloc_fn)(sizeof(RpbLink));
        if (pblink[i] == NULL) {
            return ERIAK_OUT_OF_MEMORY;
        }
        memset(pblink[i], '\0', sizeof(RpbLink));
        if (link[i]->has_bucket) {
            pblink[i]->has_bucket = RIAK_TRUE;
            riak_binary_to_pb_copy(pblink[i]->bucket, link[i]->bucket);
        }
        if (link[i]->has_key) {
            pblink[i]->has_key = RIAK_TRUE;
            riak_binary_to_pb_copy(pblink[i]->key, link[i]->key);
        }
        if (link[i]->has_tag) {
            pblink[i]->has_tag = RIAK_TRUE;
            riak_binary_to_pb_copy(pblink[i]->tag, link[i]->tag);
        }
    }
    // Finally assign the pointer to the list of link pointers
    *pblink_target = pblink;

    return ERIAK_OK;
}

static void
riak_links_free_pb(riak_context* ctx,
                   RpbLink*** pblink_target,
                   int num_links) {
    RpbLink **pblink = *pblink_target;
    int i;
    for(i = 0; i < num_links; i++) {
        riak_free(ctx, pblink[i]);
    }
    riak_free(ctx, pblink);
    *pblink_target = NULL;
}

static riak_error
riak_links_copy_from_pb(riak_context *ctx,
                        riak_link ***link_target,
                        RpbLink     **pblink,
                        int           num_links) {
    riak_link **link = (riak_link**)(ctx->malloc_fn)(sizeof(riak_link*) * num_links);
    if (pblink == NULL) {
        return ERIAK_OUT_OF_MEMORY;
    }
    int i;
    for(i = 0; i < num_links; i++) {
        link[i] = (riak_link*)(ctx->malloc_fn)(sizeof(riak_link));
        if (link[i] == NULL) {
            return ERIAK_OUT_OF_MEMORY;
        }
        memset(link[i], '\0', sizeof(riak_link));
        if (pblink[i]->has_bucket) {
            link[i]->has_bucket = RIAK_TRUE;
            riak_binary_from_pb_copy(link[i]->bucket, pblink[i]->bucket);
        }
        if (pblink[i]->has_key) {
            pblink[i]->has_key = RIAK_TRUE;
            riak_binary_from_pb_copy(link[i]->key, pblink[i]->key);
        }
        if (pblink[i]->has_tag) {
            pblink[i]->has_tag = RIAK_TRUE;
            riak_binary_from_pb_copy(link[i]->tag, pblink[i]->tag);
        }
    }
    // Finally assign the pointer to the list of link pointers
    *link_target = link;

    return ERIAK_OK;
}

static void
riak_links_free(riak_context *ctx,
                riak_link  ***link_target,
                int           num_links) {
    riak_link **link = *link_target;
    int i;
    for(i = 0; i < num_links; i++) {
        riak_free(ctx, link[i]);
    }
    riak_free(ctx, link);
    *link_target = NULL;
}

int
riak_links_print(riak_link    *link,
                 char         *target,
                 riak_uint32_t len) {
    int total = 0;
    int wrote = 0;
    char buffer[2048];
    if (link->has_bucket) {
        riak_binary_print(link->bucket, buffer, sizeof(buffer));
        wrote = snprintf(target, len, "Link buffer: %s\n", buffer);
        len -= wrote;
        target += wrote;
        total += wrote;
    }
    if (link->has_key) {
        riak_binary_print(link->key, buffer, sizeof(buffer));
        wrote = snprintf(target, len, "Link Key: %s\n", buffer);
        len -= wrote;
        target += wrote;
        total += wrote;
    }
    if (link->has_tag) {
        riak_binary_print(link->tag, buffer, sizeof(buffer));
        wrote = snprintf(target, len, "Link Tag: %s\n", target);
        len -= wrote;
        target += wrote;
        total += wrote;
    }
    return total;
}

//
// R I A K   O B J E C T
//
riak_object*
riak_object_new(riak_context *ctx) {
    riak_object *o = (riak_object*)(ctx->malloc_fn)(sizeof(riak_object));
    if (o) memset(o, '\0', sizeof(riak_object));
    return o;
}

riak_error
riak_object_new_array(riak_context  *ctx,
                      riak_object ***array,
                      riak_size_t    len) {
    riak_object **result = (riak_object**)(ctx->malloc_fn)(sizeof(riak_object)*len);
    if (result == NULL) {
        return ERIAK_OUT_OF_MEMORY;
    }
    memset((void*)result, '\0', sizeof(riak_object)*len);
    *array = result;

    return ERIAK_OK;
}

void
riak_object_free_array(riak_context  *ctx,
                       riak_object ***array,
                       riak_size_t    len) {
    int i;
    for(i = 0; i < len; i++) {
        riak_object *obj = (*array)[i];
        if (obj->n_indexes > 0) riak_pairs_free(ctx, &(obj->indexes), obj->n_indexes);
        if (obj->n_usermeta > 0) riak_pairs_free(ctx, &(obj->usermeta), obj->n_usermeta);
        if (obj->n_links > 0) riak_links_free(ctx, &(obj->links), obj->n_links);
        riak_free_ptr(ctx, obj);
    }
    riak_free_ptr(ctx, array);
}

int
riak_object_to_pb_copy(riak_context *ctx,
                       RpbContent   *to,
                       riak_object  *from) {

    rpb_content__init(to);

    riak_binary_to_pb_copy(to->value, from->value);
    if (from->has_charset) {
        to->has_charset = RIAK_TRUE;
        riak_binary_to_pb_copy(to->charset, from->charset);
    }
    if (from->has_content_encoding) {
        to->has_content_encoding = RIAK_TRUE;
        riak_binary_to_pb_copy(to->content_encoding, from->encoding);
    }
    if (from->has_content_type) {
        to->has_content_type = RIAK_TRUE;
        riak_binary_to_pb_copy(to->content_type, from->content_type);
    }
    if (from->has_deleted) {
        to->has_deleted = RIAK_TRUE;
        to->deleted = from->deleted;
    }
    if (from->has_last_mod) {
        to->has_last_mod = RIAK_TRUE;
        to->last_mod = from->last_mod;
    }
    if (from->has_last_mod_usecs) {
        to->has_last_mod_usecs = RIAK_TRUE;
        to->last_mod_usecs = from->last_mod_usecs;
    }
    if (from->has_vtag) {
        to->has_vtag = RIAK_TRUE;
        riak_binary_to_pb_copy(to->vtag, from->vtag);
    }

    // Indexes
    if (from->n_indexes > 0) {
        to->n_indexes = from->n_indexes;
        int idxresult = riak_pairs_copy_to_pb(ctx, &(to->indexes), from->indexes, to->n_indexes);
        if (idxresult) {
            return idxresult;
        }
    }

    // User-Metadave
    if (from->n_usermeta > 0) {
        to->n_usermeta = from->n_usermeta;
        riak_error err = riak_pairs_copy_to_pb(ctx, &(to->usermeta), from->usermeta, to->n_usermeta);
        if (err) {
            return err;
        }
    }

    // Links
    if (from->n_links > 0) {
        to->n_links = from->n_links;
        riak_error err = riak_links_copy_to_pb(ctx, &(to->links), from->links, to->n_links);
        if (err) {
            return err;
        }
    }

    return ERIAK_OK;
}


riak_error
riak_object_new_from_pb(riak_context *ctx,
                        riak_object **target,
                        RpbContent   *from) {
    *target = riak_object_new(ctx);
    if (*target == NULL) {
        return ERIAK_OUT_OF_MEMORY;
    }
    riak_object *to = *target;

    riak_binary_from_pb_copy(to->value, from->value);
    if (from->has_charset) {
        to->has_charset = RIAK_TRUE;
        riak_binary_from_pb_copy(to->charset, from->charset);
    }
    if (from->has_content_encoding) {
        to->has_content_encoding = RIAK_TRUE;
        riak_binary_from_pb_copy(to->encoding, from->content_encoding);
    }
    if (from->has_content_type) {
        to->has_content_type = RIAK_TRUE;
        riak_binary_from_pb_copy(to->content_type, from->content_type);
    }
    if (from->has_deleted) {
        to->has_deleted = RIAK_TRUE;
        to->deleted = from->deleted;
    }
    if (from->has_last_mod) {
        to->has_last_mod = RIAK_TRUE;
        to->last_mod = from->last_mod;
    }
    if (from->has_last_mod_usecs) {
        to->has_last_mod_usecs = RIAK_TRUE;
        to->last_mod_usecs = from->last_mod_usecs;
    }
    if (from->has_vtag) {
        to->has_vtag = RIAK_TRUE;
        riak_binary_from_pb_copy(to->vtag, from->vtag);
    }

    // Indexes
    if (from->n_indexes > 0) {
        to->n_indexes = from->n_indexes;
        int idxresult = riak_pairs_copy_from_pb(ctx, &(to->indexes), from->indexes, to->n_indexes);
        if (idxresult) {
            return idxresult;
        }
    }

    // User-Metadave
    if (from->n_usermeta > 0) {
        to->n_usermeta = from->n_usermeta;
        int uresult = riak_pairs_copy_from_pb(ctx, &(to->usermeta), from->usermeta, to->n_usermeta);
        if (uresult) {
            return uresult;
        }
    }

    // Links
    if (from->n_links > 0) {
        to->n_links = from->n_links;
        int lresult = riak_links_copy_from_pb(ctx, &(to->links), from->links, to->n_links);
        if (lresult) {
            return lresult;
        }
    }
    return ERIAK_OK;
}

int
riak_object_print(riak_object  *obj,
                  char         *target,
                  riak_uint32_t len) {
    char buffer[2048];
    riak_binary_print(obj->bucket, buffer, sizeof(buffer));
    int total = 0;
    int wrote = snprintf(target, len, "Bucket: %s\n", buffer);
    len -= wrote;
    target += wrote;
    total += wrote;
    if (obj->has_key) {
        riak_binary_print(obj->key, buffer, sizeof(buffer));
        wrote = snprintf(target, len, "Key: %s\n", buffer);
        len -= wrote;
        target += wrote;
        total += wrote;
    }
    // TODO: Bigger buffer for full value
    riak_binary_print(obj->value, buffer, sizeof(buffer));
    wrote = snprintf(target, len, "Value: %s\n", buffer);
    len -= wrote;
    target += wrote;
    total += wrote;
    if (obj->has_charset) {
        riak_binary_print(obj->charset, buffer, sizeof(buffer));
        wrote = snprintf(target, len, "Charset: %s\n", buffer);
        len -= wrote;
        target += wrote;
        total += wrote;
    }
    if (obj->has_last_mod) {
        time_t mod = (time_t)(obj->last_mod);
        strftime(buffer, 1024, "%Y-%m-%d %H:%M:%S", localtime(&mod));
        wrote = snprintf(target, len, "Last Mod: %s\n", buffer);
        len -= wrote;
        target += wrote;
        total += wrote;
    }
    if (obj->has_last_mod_usecs) {
        wrote = snprintf(target, len, "Last Mod uSecs: %d\n", obj->last_mod_usecs);
        len -= wrote;
        target += wrote;
        total += wrote;
    }
    if (obj->has_content_type) {
        riak_binary_print(obj->content_type, buffer, sizeof(buffer));
        wrote = snprintf(target, len, "Content Type: %s\n", buffer);
        len -= wrote;
        target += wrote;
        total += wrote;
    }
    if (obj->has_content_encoding) {
        riak_binary_print(obj->encoding, buffer, sizeof(buffer));
        wrote = snprintf(target, len, "Content Encoding: %s\n", buffer);
        len -= wrote;
        target += wrote;
        total += wrote;
    }
    if (obj->has_deleted) {
        wrote = snprintf(target, len, "Deleted: %s\n", (obj->deleted) ? "true" : "false");
        len -= wrote;
        target += wrote;
        total += wrote;
    }
    if (obj->has_vtag) {
        riak_binary_print(obj->vtag, buffer, sizeof(buffer));
        wrote = snprintf(target, len, "VTag: %s\n", buffer);
        len -= wrote;
        target += wrote;
        total += wrote;
    }

    int i;
    for(i = 0; i < obj->n_links; i++) {
        wrote = riak_links_print(obj->links[i], target, len);
        len -= wrote;
        target += wrote;
        total += wrote;
    }
    return total;
}

void
riak_object_free(riak_context *ctx,
                 riak_object  *obj) {
    riak_pairs_free(ctx, &(obj->indexes), obj->n_indexes);
    riak_pairs_free(ctx, &(obj->usermeta), obj->n_usermeta);
    riak_links_free(ctx, &(obj->links), obj->n_links);
}

void
riak_object_free_pb(riak_context *ctx,
                    RpbContent   *obj) {
    riak_pairs_free_pb(ctx, &(obj->indexes), obj->n_indexes);
    riak_pairs_free_pb(ctx, &(obj->usermeta), obj->n_usermeta);
    riak_links_free_pb(ctx, &(obj->links), obj->n_links);
}
