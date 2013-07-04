#include "riak.h"
#include "riak_utils.h"
#include "riak_pb_message.h"


void _riak_free(void **pp) {
    if(pp != NULL && *pp != NULL) {
        free(*pp);
        *pp = NULL;
    }
}

// RIAK_OBJECT
riak_object *riak_object_new() {
    riak_object *o = (riak_object*)malloc(sizeof(riak_object));
    // TODO: check malloc return status
    // TODO: do I need bzero?
    bzero(o, sizeof(riak_object));
    return o;
}

void riak_object_free(riak_object *o) {
    if (o == NULL) {
        return;
    }
    riak_free(o);
}

// RIAK_BINARY
// be careful
riak_binary *riak_binary_new(riak_size_t len, riak_uint8_t *data) {
      riak_binary *b = (riak_binary*)malloc(sizeof(riak_binary));
      b->len  = len;
      b->data = (riak_uint8_t*)malloc(len);
      // TODO: Check malloc return status
      memcpy((void*)b->data, (void*)data, len);
      return b;
}

void riak_binary_populate(riak_binary *b, riak_size_t len, riak_uint8_t *data) {
      b->len  = len;
      b->data = (uint8_t*)malloc(len);
      // TODO: Check malloc return status
      memcpy((void*)b->data, (void*)data, len);
}

void riak_binary_free(riak_binary *b) {
      if (b == NULL) {
          return;
      }
      riak_free(b->data);
      riak_free(b);
}

void riak_binary_copy(riak_binary* to, riak_binary* from) {
    to->len  = from->len;
    to->data = from->data;
}

void riak_binary_deep_copy(riak_binary *to, riak_binary *from) {
    to->len  = from->len;
    to->data = (riak_uint8_t*)malloc(from->len);
    // TODO: Check malloc return status
    memcpy((void*)to->data, (void*)from->data, from->len);
}


riak_response* riak_response_new() {
    riak_response* r = (riak_response*)malloc(sizeof(riak_response));
    bzero(r, sizeof(riak_response));
    return r;
}

void riak_response_free(riak_response *r) {
    if(r == 0) {
        return;
    }
    if(r->object_count > 0) {
        //for(i = 0; i < r->object_count; i++) {
        //  free_riak_object(&r->objects[i]);
        //}
    }
    riak_free(r);
}

/* LIBEVENT CALLBACKS */

void eventcb(struct bufferevent *bev, short events, void *ptr)
{
    if (events & BEV_EVENT_CONNECTED) {
         fprintf(stderr, "Connect okay.\n");
    } else if (events & (BEV_EVENT_ERROR|BEV_EVENT_EOF)) {
         struct event_base *base = ptr;
         if (events & BEV_EVENT_ERROR) {
            int err = bufferevent_socket_get_dns_error(bev);
            if (err)
                 printf("DNS error: %s\n", evutil_gai_strerror(err));
         }
         fprintf(stderr, "Closing\n");
         bufferevent_free(bev);
         event_base_loopexit(base, NULL);
    } else {
        fprintf(stderr, "Event %d\n", events);
    }
}

int riak_send_req(riak_context *ctx, riak_uint8_t reqid, riak_uint8_t *msgbuf, riak_size_t len) {
    riak_bufferevent *bev = ctx->bevent;
    // Convert len to network byte order
    ev_uint32_t msglen = htonl(len+1);
    int result = bufferevent_write(bev, (void*)&msglen, sizeof(msglen));
    if (result != 0) return 1;
    result = bufferevent_write(bev, (void*)&reqid, sizeof(reqid));
    if (result != 0) return 1;
    if (msglen > 0) {
        result = bufferevent_write(bev, (void*)msgbuf, len);
        if (result != 0) return 1;
    }
    fprintf(stderr, "Wrote %d bytes\n", (int)len);
    return 0;
}

void riak_read_result_callback(riak_bufferevent *bev, void *ptr) {
    riak_uint32_t inmsglen;
    riak_size_t buflen = bufferevent_read(bev, (void*)&inmsglen, sizeof(inmsglen));
    assert(buflen == 4);
    riak_uint32_t msglen = ntohl(inmsglen);
    riak_uint8_t *buffer = (riak_uint8_t*)malloc(msglen);
    assert(buffer != 0);
    buflen = bufferevent_read(bev, (void*)buffer, msglen);
    assert(buflen == msglen);
    riak_uint8_t msgtype = *buffer;
    fprintf(stderr, "Read msg [%d] %d of %d bytes\n", (unsigned char)msgtype, (int)msglen, (int)buflen);
    // Pull off packing

    if (msgtype == MSG_RPBERRORRESP) {
        RpbErrorResp *errresp= rpb_error_resp__unpack(NULL, buflen-1, (uint8_t*)(buffer+1));
        fprintf(stderr, "Err Unpacked\n");
        uint32_t errcode = errresp->errcode;
        ProtobufCBinaryData binary = errresp->errmsg;
        char errmsg[8192];
        fprintf(stderr, "len = %d\n", (int)binary.len);
        strncpy(errmsg, (char*)binary.data, binary.len);
        errmsg[binary.len] = '\0';
        fprintf(stderr, "ERR #%d - %s\n", errcode, errmsg);
        exit(1);
    }
    RpbListBucketsResp *listbucketresp = rpb_list_buckets_resp__unpack(NULL /*allocator*/, buflen-1, (uint8_t*)(buffer+1));
    fprintf(stderr, "Unpacked\n");
    int i;
    char name[1024];
    for(i = 0; i < listbucketresp->n_buckets; i++) {
        ProtobufCBinaryData binary = listbucketresp->buckets[i];
        strncpy(name, (const char*)binary.data, binary.len);
        name[binary.len] = '\0';
        printf("%d - %s\n", i, name);
    }
    rpb_list_buckets_resp__free_unpacked(listbucketresp, NULL /*allocator*/);
    fprintf(stderr,"Freed\n");
}
