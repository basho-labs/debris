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
#include "riak.h"
#include "riak_utils.h"

// RIAK_OBJECT
riak_object *riak_object_new(riak_context *ctx) {
    riak_object *o = (riak_object*)(ctx->malloc_fn)(sizeof(riak_object));
    // TODO: check malloc return status
    // TODO: do I need bzero?
    bzero(o, sizeof(riak_object));
    return o;
}

static int riak_copy_pairs_to_pb(int num_pairs, riak_context* ctx, RpbPair*** pbpair_target, riak_pair **pair) {
    RpbPair **pbpair = (RpbPair**)(ctx->malloc_fn)(sizeof(RpbPair*) * num_pairs);
    if (pbpair == NULL) {
        // TODO: Error handling
        return 1;
    }
    int i;
    for(i = 0; i < num_pairs; i++) {
        pbpair[i] = (RpbPair*)(ctx->malloc_fn)(sizeof(RpbPair));
        if (pbpair[i] == NULL) {
            // TODO: Error handling
            return 1;
        }
        memset(pbpair[i], '\0', sizeof(RpbPair));
        if (pair[i]->has_value) {
            pbpair[i]->has_value = RIAK_TRUE;
            riak_binary_to_pb_copy(pbpair[i]->value, pair[i]->value);
        }
    }
    // Finally assign the pointer to the list of pair pointers
    *pbpair_target = pbpair;
    return 0;
}

static void riak_free_pairs_pb(int num_pairs, riak_context* ctx, RpbPair*** pbpair_target) {
    RpbPair **pbpair = *pbpair_target;
    int i;
    for(i = 0; i < num_pairs; i++) {
        riak_free(ctx, pbpair[i]);
    }
    riak_free(ctx, pbpair);
    *pbpair_target = NULL;
}

static int riak_copy_pairs_from_pb(int num_pairs, riak_context* ctx, riak_pair*** pair_target, RpbPair **pbpair) {
    riak_pair **pair = (riak_pair**)(ctx->malloc_fn)(sizeof(riak_pair*) * num_pairs);
    if (pair == NULL) {
        // TODO: Error handling
        return 1;
    }
    int i;
    for(i = 0; i < num_pairs; i++) {
        pair[i] = (riak_pair*)(ctx->malloc_fn)(sizeof(riak_pair));
        if (pair[i] == NULL) {
            // TODO: Error handling
            return 1;
        }
        memset(pair[i], '\0', sizeof(riak_pair));
        if (pbpair[i]->has_value) {
            pair[i]->has_value = RIAK_TRUE;
            riak_binary_from_pb_copy(pair[i]->value, pbpair[i]->value);
        }
    }
    // Finally assign the pointer to the list of pair pointers
    *pair_target = pair;
    return 0;
}

static void riak_free_pairs(int num_pairs, riak_context* ctx, riak_pair*** pair_target) {
    riak_pair **pair = *pair_target;
    int i;
    for(i = 0; i < num_pairs; i++) {
        riak_free(ctx, pair[i]);
    }
    riak_free(ctx, pair);
    *pair_target = NULL;
}

static int riak_copy_links_to_pb(int num_links, riak_context* ctx, RpbLink*** pblink_target, riak_link **link) {
    RpbLink **pblink = (RpbLink**)(ctx->malloc_fn)(sizeof(RpbLink*) * num_links);
    if (pblink == NULL) {
        // TODO: Error handling
        return 1;
    }
    int i;
    for(i = 0; i < num_links; i++) {
        pblink[i] = (RpbLink*)(ctx->malloc_fn)(sizeof(RpbLink));
        if (pblink[i] == NULL) {
            // TODO: Error handling
            return 1;
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
    return 0;
}

static void riak_free_links_pb(int num_links, riak_context* ctx, RpbLink*** pblink_target) {
    RpbLink **pblink = *pblink_target;
    int i;
    for(i = 0; i < num_links; i++) {
        riak_free(ctx, pblink[i]);
    }
    riak_free(ctx, pblink);
    *pblink_target = NULL;
}

static int riak_copy_links_from_pb(int num_links, riak_context* ctx, riak_link*** link_target, RpbLink **pblink) {
    riak_link **link = (riak_link**)(ctx->malloc_fn)(sizeof(riak_link*) * num_links);
    if (pblink == NULL) {
        // TODO: Error handling
        return 1;
    }
    int i;
    for(i = 0; i < num_links; i++) {
        link[i] = (riak_link*)(ctx->malloc_fn)(sizeof(riak_link));
        if (link[i] == NULL) {
            // TODO: Error handling
            return 1;
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
    return 0;
}

static void riak_free_links(int num_links, riak_context* ctx, riak_link*** link_target) {
    riak_link **link = *link_target;
    int i;
    for(i = 0; i < num_links; i++) {
        riak_free(ctx, link[i]);
    }
    riak_free(ctx, link);
    *link_target = NULL;
}

int riak_object_to_pb_copy(riak_context *ctx, RpbContent *to, riak_object *from) {

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
        int idxresult = riak_copy_pairs_to_pb(to->n_indexes, ctx, &(to->indexes), from->indexes);
        if (idxresult) {
            return idxresult;
        }
    }

    // User-Metadave
    if (from->n_usermeta > 0) {
        to->n_usermeta = from->n_usermeta;
        int uresult = riak_copy_pairs_to_pb(to->n_usermeta, ctx, &(to->usermeta), from->usermeta);
        if (uresult) {
            return uresult;
        }
    }

    // Links
    if (from->n_links > 0) {
        to->n_links = from->n_links;
        int lresult = riak_copy_links_to_pb(to->n_links, ctx, &(to->links), from->links);
        if (lresult) {
            return lresult;
        }
    }

    return 0;
}

int riak_object_from_pb_copy(riak_context *ctx, riak_object* to, RpbContent* from) {
    memset((void*)to, '\0', sizeof(riak_object));

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
        int idxresult = riak_copy_pairs_from_pb(to->n_indexes, ctx, &(to->indexes), from->indexes);
        if (idxresult) {
            return idxresult;
        }
    }

    // User-Metadave
    if (from->n_usermeta > 0) {
        to->n_usermeta = from->n_usermeta;
        int uresult = riak_copy_pairs_from_pb(to->n_usermeta, ctx, &(to->usermeta), from->usermeta);
        if (uresult) {
            return uresult;
        }
    }

    // Links
    if (from->n_links > 0) {
        to->n_links = from->n_links;
        int lresult = riak_copy_links_from_pb(to->n_links, ctx, &(to->links), from->links);
        if (lresult) {
            return lresult;
        }
    }
    return 0;
}

void riak_object_free(riak_context *ctx, riak_object* obj) {
    riak_free_pairs(obj->n_indexes, ctx, &(obj->indexes));
    riak_free_pairs(obj->n_usermeta, ctx, &(obj->usermeta));
    riak_free_links(obj->n_links, ctx, &(obj->links));
}

void riak_object_free_pb(riak_context *ctx, RpbContent* obj) {
    riak_free_pairs_pb(obj->n_indexes, ctx, &(obj->indexes));
    riak_free_pairs_pb(obj->n_usermeta, ctx, &(obj->usermeta));
    riak_free_links_pb(obj->n_links, ctx, &(obj->links));
}
