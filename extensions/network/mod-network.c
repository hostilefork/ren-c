//
//  File: %mod-network.c
//  Summary: "network port interface"
//  Section: ports
//  Project: "Rebol 3 Interpreter and Run-time (Ren-C branch)"
//  Homepage: https://github.com/metaeducation/ren-c/
//
//=/////////////////////////////////////////////////////////////////////////=//
//
// Copyright 2012 REBOL Technologies
// Copyright 2012-2021 Ren-C Open Source Contributors
// REBOL is a trademark of REBOL Technologies
//
// See README.md and CREDITS.md for more information.
//
// Licensed under the Lesser GPL, Version 3.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// https://www.gnu.org/licenses/lgpl-3.0.html
//
//=/////////////////////////////////////////////////////////////////////////=//
//
// This is a TCP networking interface, evolved from the R3-Alpha PORT! code.
// It has been rewritten in terms of libuv as a platform abstraction layer.
// Although libuv has the implementation capability to do non-blocking and
// parallel network operations, the goal in Ren-C is to push the language
// toward allowing people to express their code in a synchronous fashion and
// have the asynchronous behavior accomplished by more "modern" means...taking
// inspiration from Go's goroutines and async/await:
//
//   https://forum.rebol.info/t/1733
//
//=//// NOTES //////////////////////////////////////////////////////////////=//
//
// * Although some libuv APIs (such as the filesystem) allow you to pass in
//   nullptr for the callback and get synchronous behavior, the network APIs
//   don't seem to do that.  Hence the synchronous behavior is achieved here
//   by having operations like READ or WRITE call the event loop until they
//   notice they have completed.
//

#include "uv.h"  // includes windows.h
#ifdef TO_WINDOWS
    #undef OUT  // %minwindef.h defines this, we have a better use for it
    #undef VOID  // %winnt.h defines this, we have a better use for it
#endif

#include "sys-core.h"

#include "reb-net.h"
extern REBVAL *rebError_UV(int err);

#include "tmp-mod-network.h"


#define NET_BUF_SIZE 32*1024


static void Set_Addr(struct sockaddr_in *sa, long ip, int port)
{
    // Set the IP address and port number in a socket_addr struct.

    memset(sa, 0, sizeof(*sa));
    sa->sin_family = AF_INET;

    // htonl(ip); NOTE: REBOL stays in network byte order
    //
    sa->sin_addr.s_addr = ip;
    sa->sin_port = htons((unsigned short)port);
}

static void Get_Local_IP(SOCKREQ *sock)
{
    // Get the local IP address and port number.
    // This code should be fast and never fail.

    struct sockaddr_in sa;
    int len = sizeof(sa);

    uv_tcp_getsockname(&sock->tcp, cast(struct sockaddr *, &sa), &len);
    assert(len == sizeof(sa));

    // htonl(ip); NOTE: REBOL stays in network byte order
    //
    sock->local_ip = sa.sin_addr.s_addr;
    sock->local_port_number = ntohs(sa.sin_port);
}


//
//  Open_Socket: C
//
// Setup a socket with the specified protocol and bind it to the related
// transport service.
//
// Note: No actual connection is made by calling this routine.  The IP address
// and port number are not needed, only the type of service required.
//
// After usage:
//     Close_Socket() - to free OS allocations
//
REBVAL *Open_Socket(const REBVAL *port)
{
    SOCKREQ *sock = Sock_Of_Port(port);
    assert(sock->stream == nullptr);

    sock->modes = 0;  // clear all flags

    assert(sock->transport == TRANSPORT_TCP);  // different UDP libuv functions

    int r = uv_tcp_init_ex(uv_default_loop(), &sock->tcp, AF_INET);
    if (r < 0)
        return rebError_UV(r);

    sock->stream = cast(uv_stream_t*, &sock->tcp);  // signal tcp is set

    return nullptr;
}


void on_close(uv_handle_t *handle) {
    bool *finished = cast(bool*, handle->data);
    *finished = true;
}

static void Close_Sock_If_Needed(SOCKREQ* sock) {
    if (sock->stream) {
        bool finished;
        sock->tcp.data = &finished;
        uv_close(cast(uv_handle_t*, &sock->tcp), on_close);

        do {
            uv_run(uv_default_loop(), UV_RUN_ONCE);
        } while (not finished);

        sock->stream = nullptr;
        sock->modes = 0;
    }
}

static void cleanup_sockreq(const REBVAL *v) {
    SOCKREQ* sock = VAL_HANDLE_POINTER(SOCKREQ, v);
    Close_Sock_If_Needed(sock);
    Free(SOCKREQ, sock);
}


//
//  Close_Socket: C
//
REBVAL *Close_Socket(const REBVAL *port)
{
    SOCKREQ* sock = Sock_Of_Port(port);

    REBVAL *error = nullptr;

    // Note: R3-Alpha allowed closing closed sockets
    Close_Sock_If_Needed(sock);

    return error;
}


//
//  Lookup_Socket_Synchronously: C
//
// !!! R3-Alpha would use asynchronous DNS API on Windows, but that API
// was not supported by IPv6, and developers are encouraged to use normal
// socket APIs with their own threads.  Now that we use libuv, there is again
// the ability to specify a callback and do asynchronous lookup.  But this
// would have to be fit in with a client understanding for how to request a
// LOOKUP event, and when it had to be waited on.  For now it's synchronous.
//
REBVAL *Lookup_Socket_Synchronously(
    const REBVAL *port,
    const REBVAL *hostname
){
    SOCKREQ *sock = Sock_Of_Port(port);

    assert(Is_Text(hostname));
    const char *hostname_utf8 = cs_cast(Cell_Utf8_At(hostname));
    char *port_number_utf8 = rebSpell(
        Canon(FORM), rebI(sock->remote_port_number)
    );

    // !!! You can leave the "hints" argument as nullptr.  But this is what
    // Julia said for hints, which didn't prescribe an ai_family of PF_INET,
    // and it also used memset() to 0...so it got hints.ai_protocol as
    // IPPROTO_IP which is called a "dummy for IP":
    //
    //     struct addrinfo hints;
    //     memset(&hints, 0, sizeof(hints));
    //     hints.ai_family = PF_UNSPEC;
    //     hints.ai_socktype = SOCK_STREAM;
    //     hints.ai_flags |= AI_CANONNAME;
    //
    // The example in libuv's documentation was more specific and did not
    // bother with the memset...but it set the ai_protocol.  For starters we
    // use the simpler-seeming libuv case.
    //
    struct addrinfo hints;
    hints.ai_family = PF_INET;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_protocol = IPPROTO_TCP;
    hints.ai_flags = 0;

    // This is a replacement for:
    //
    //     HOSTENT *host = gethostbyname(cs_cast(hostname_utf8));
    //
    uv_getaddrinfo_t req;
    int r = uv_getaddrinfo(
        uv_default_loop(),
        &req,
        nullptr,  // callback
        hostname_utf8,  // called "node" in libuv, but "hostname" in POSIX
        port_number_utf8,  // "service" string or port (e.g. "echo", "80")
        &hints  // "hints" (which is a const struct addrinfo*)
    );

    rebFree(port_number_utf8);

    if (r != 0)
        return rebError_UV(r);

    // Synchronously fill in the port's remote_ip with the answer to looking
    // up the hostname.
    //
    // This is a replacement for:
    //
    //      memcpy(&sock->remote_ip, *host->h_addr_list, 4);
    //
    struct sockaddr_in *sa = cast(struct sockaddr_in*, req.addrinfo->ai_addr);

    // https://stackoverflow.com/q/31343855/
    //
    assert(req.addrinfo->ai_addrlen == 16);
    assert(sizeof(sa->sin_addr) == 4);
    memcpy(&sock->remote_ip, &sa->sin_addr, 4);

    uv_freeaddrinfo(req.addrinfo);  // have to free it

    // !!! Theoretically this is where we'd know whether it's an IPv6 address
    // or an IPv4 address.  This is still transitional IPv4 code, though.

    return nullptr;
}


// This libuv callback is triggered when a Request_Connect_Socket()
// connection has been made...or an error is raised.
//
// The callback will only be invoked when the libuv event loop is being run.
//
static void on_connect(uv_connect_t *req, int status) {
    Reb_Connect_Request *rebreq = cast(Reb_Connect_Request*, req);
    const REBVAL *port = CTX_ARCHETYPE(rebreq->port_ctx);
    SOCKREQ *sock = Sock_Of_Port(port);

    if (status < 0) {
        rebreq->result = rebError_UV(status);
    }
    else {
        sock->stream = req->handle;

        Get_Local_IP(sock);
        rebreq->result = rebBlank();
    }
}


//
//  Request_Connect_Socket: C
//
// Connect a socket to a service.
// Only required for connection-based protocols (e.g. not UDP).
// The IP address must already be resolved before calling.
//
// This function is asynchronous. It will return immediately.
// You can call this function again to check the pending connection.
//
// Before usage:
//     Open_Socket() -- to allocate the socket
//
REBVAL *Request_Connect_Socket(const REBVAL *port)
{
    SOCKREQ *sock = Sock_Of_Port(port);
    assert(not (sock->modes & RST_LISTEN));

    struct sockaddr_in sa;

    Set_Addr(&sa, sock->remote_ip, sock->remote_port_number);

    // !!! For some reason the on_connect() callback cannot be passed as
    // nullptr to get a synchronous connection.
    //
    Reb_Connect_Request *rebreq = rebAlloc(Reb_Connect_Request);
    rebreq->port_ctx = VAL_CONTEXT(port);  // !!! keepalive as API handle?
    rebreq->result = nullptr;

    int r = uv_tcp_connect(
        &rebreq->req, &sock->tcp, cast(struct sockaddr *, &sa), on_connect
    );

    if (r < 0) {  // the *request* failed (didn't even try to connect)
        Close_Socket(port);
        return rebError_UV(r);
    }

    do {
        uv_run(uv_default_loop(), UV_RUN_ONCE);
    } while (rebreq->result == nullptr);

    if (not Is_Blank(rebreq->result))
        fail (rebreq->result);
    rebRelease(rebreq->result);

    rebFree(rebreq);

    return nullptr;
}


//
// Accept an inbound connection on a TCP listen socket.
//
void on_new_connection(uv_stream_t *server, int status) {
    Context* listener_port_ctx = cast(Context*, server->data);
    const REBVAL *listening_port = CTX_ARCHETYPE(listener_port_ctx);
    SOCKREQ *listening_sock = Sock_Of_Port(listening_port);
    UNUSED(listening_sock);

    // !!! This connection can happen at any time the libUV event loop runs.
    // So this error has a chance of being raised during unrelated READ or
    // WRITE calls.  How should such errors be delivered?
    //
    if (status < 0)
        fail (rebError_UV(status));

    Context* client = Copy_Context_Shallow_Managed(listener_port_ctx);
    Push_GC_Guard(client);

    Init_Nulled(CTX_VAR(client, STD_PORT_DATA));  // just to be sure

    REBVAL *c_state = CTX_VAR(client, STD_PORT_STATE);
    SOCKREQ* sock = Try_Alloc(SOCKREQ);
    memset(sock, 0, sizeof(SOCKREQ));

    Init_Handle_Cdata_Managed(c_state, sock, sizeof(SOCKREQ), &cleanup_sockreq);

    SOCKREQ *sock_new = Sock_Of_Port(CTX_ARCHETYPE(client));

    // Create a new port using ACCEPT

    uv_tcp_init(uv_default_loop(), &sock_new->tcp);
    sock_new->stream = cast(uv_stream_t*, &sock_new->tcp);

    int r = uv_accept(server, sock_new->stream);
    if (r < 0)
        fail (rebError_UV(r));  // !!! See note on FAIL above about errors here

    // NOTE: REBOL stays in network byte order, no htonl(ip) needed
    //
    struct sockaddr_in sa;
    int len = sizeof(sa);
    uv_tcp_getpeername(&sock_new->tcp, cast(struct sockaddr *, &sa), &len);
    assert(len == sizeof(sa));
    sock_new->remote_ip = sa.sin_addr.s_addr;
    sock_new->remote_port_number = ntohs(sa.sin_port);

    Get_Local_IP(sock_new);

    Drop_GC_Guard(client);

    rebElide("(", listening_port, ").spec.accept", CTX_ARCHETYPE(client));
}


//
//  Start_Listening_On_Socket: C
//
// Setup a listening TCP socket.
//
// Before usage:
//     Open_Socket();
//     Set local_port to desired port number.
//
// Use this instead of Connect_Socket().
//
// !!! Historically this was common for TCP and UDP.  libuv separates the
// bind() command to operate on different types, there is a tcp_t vs. udp_t
// for the socket itself.
//
REBVAL *Start_Listening_On_Socket(const REBVAL *port)
{
    SOCKREQ *sock = Sock_Of_Port(port);
    sock->modes |= RST_LISTEN;

    assert(sock->stream != nullptr);  // must be open

    // Setup socket address range and port:
    //
    struct sockaddr_in sa;
    Set_Addr(&sa, INADDR_ANY, sock->local_port_number);

  blockscope {
    int r = uv_tcp_bind(&sock->tcp, cast(struct sockaddr*, &sa), 0);
    if (r < 0)
        return rebError_UV(r);
  }

  blockscope {
    sock->tcp.data = VAL_CONTEXT(port);
    int r = uv_listen(
        cast(uv_stream_t*, &sock->tcp),
        DEFAULT_BACKLOG,
        on_new_connection
    );
    if (r < 0)
        return rebError_UV(r);
  }

    sock->modes |= RSM_BIND;

    Get_Local_IP(sock);

    return nullptr;
}


// libuv actually enforces allocating a buffer on each read request, and it
// gives a suggested size which can be large (64k) in all cases, no matter
// how much you are asking to read.
//
// https://stackoverflow.com/questions/28511541/
//
// With memory pooling the cost of this can be basically nothing compared to
// the cost of network transfers, but beyond that point...it means that there
// is no interface for limiting the amount of data read besides limiting the
// size of the buffer.
//
// !!! In R3-Alpha, the client could leave data in the buffer of the port and
// just accumulate it, as in SYNC-OP from %prot-http.r:
//
//     while [not find [ready close] state.state] [
//         if not port? wait [state.connection port.spec.timeout] [
//             fail make-http-error "Timeout"
//         ]
//         if state.state = 'reading-data [
//             read state.connection
//         ]
//     ]
//
// So for transitional compatibility with R3-Alpha ports, data is accrued in
// the `data` field of the port as a BINARY!.  This adds up over successive
// reads until the port clears it.
//
void on_read_alloc(uv_handle_t *handle, size_t suggested_size, uv_buf_t *buf)
{
    UNUSED(suggested_size);

    Reb_Read_Request *rebreq = cast(Reb_Read_Request*, handle->data);

    Context* port_ctx = rebreq->port_ctx;
    REBVAL *port_data = CTX_VAR(port_ctx, STD_PORT_DATA);

    size_t bufsize;
    if (rebreq->length == UNLIMITED)  // read maximum amount possible
        bufsize = NET_BUF_SIZE;  // !!! use libuv's (large) suggestion instead?
    else
        bufsize = rebreq->length - rebreq->actual;  // !!! use suggestion here?

    Binary* bin;
    if (Is_Nulled(port_data)) {
        bin = Make_Binary(bufsize);
        Init_Binary(port_data, bin);
    }
    else {
        bin = Cell_Binary_Known_Mutable(port_data);

        // !!! Port code doesn't skip the index, but what if user does?
        //
        assert(VAL_INDEX(port_data) == 0);

        // !!! Binaries need +1 space for the terminator, but that is handled
        // internally to Extend_Series.  Review wasted space in array case.
        //
        Extend_Series_If_Necessary(bin, bufsize);
    }

    buf->base = s_cast(Binary_Tail(bin));
    buf->len = bufsize;

    // We are handing out a buffer of size buf->len
}


// stream-oriented libuv callback for reading.
//
// !!! The model of libuv's streaming is such that you cannot make another
// uv_read_start() request without calling uv_read_stop().  For now we stop and
// start, but the right answer is to expose an interface more attuned to how
// streaming actually works.
//
void on_read(uv_stream_t *stream, ssize_t nread, const uv_buf_t *buf)
{
    Reb_Read_Request *rebreq = cast(Reb_Read_Request*, stream->data);
    Context* port_ctx = rebreq->port_ctx;

    REBVAL *port_data = CTX_VAR(port_ctx, STD_PORT_DATA);

    Binary* bin;
    if (Is_Nulled(port_data)) {
        //
        // An error like "connection reset by peer" can occur before a call to
        // on_read_alloc() is made, so the buffer might be null in that case.
        // For safety's sake, assume this could also happen for 0 reads.
        //
        assert(nread <= 0);  // error or 0 read
        bin = nullptr;
    }
    else
        bin = Cell_Binary_Known_Mutable(port_data);

    if (nread == 0) {  // Zero bytes read
        //
        // Note: "nread might be 0, which does not indicate an error or EOF.
        // This is equivalent to EAGAIN or EWOULDBLOCK under read(2)."
        //
        // It seems like this means that the buffer you allocated would just
        // be tossed; but we're "allocating" buffers sequentially out of the
        // port's binary at the moment.  Do nothing?
    }
    else if (nread < 0) {  // Error while reading
        //
        // !!! How to handle corrupted data?  Clear the whole buffer?  Leave
        // it at the termination before the READ?  Clear it for now just to
        // catch errors where partial data would be used as if it were okay,
        // but consider a defensive strategy that could use partial results.
        // Note that on_read_alloc() may not have been called at all, as in
        // the case of some "connection reset by peer" errors; so port_data
        // might be a binary or it might be nulled.
        //
        Init_Nulled(port_data);

        // Asking to do a `uv_read_stop()` when an error happens asserts:
        // https://github.com/joyent/libuv/issues/1534

        stream->data = nullptr;

        rebreq->result = rebError_UV(nread);
    }
    else if (nread == UV_EOF) {
        //
        // Asking to do a `uv_read_stop()` when you've reached EOF asserts:
        // https://github.com/joyent/libuv/issues/1534

        if (rebreq->length == UNLIMITED) {
            //
            // -1 is the "read as much as you can" signal.  Reaching the end is
            // an acceptable outcome.
            //
            goto post_read_finished_event;
        }
        else {
            // If we had a /PART setting on the READ, we follow the Rebol
            // convention of allowing less than that to be accepted, which
            // FILE! does as well:
            //
            //     >> write %test.dat #{01}
            //
            //     >> read/part %test.dat 100000
            //     == #{01}
            //
            // Hence it is the caller's responsibility to check how much
            // data they actually got with a READ/PART call.  But this is
            // where you could handle that situation differently.

            // Note: cast of UNLIMITED to unsigned will be a very large value.
            //
            assert(rebreq->actual < cast(size_t, rebreq->length));

            goto post_read_finished_event;
        }
    }
    else {
        // Note that "each buffer is used only once", e.g. there is a call
        // to on_read_alloc() for every read.
        //
        assert(buf->base == s_cast(Binary_Tail(bin)));
        UNUSED(buf);

        rebreq->actual += nread;

        // Binaries must be kept with proper termination in case the GC sees
        // them.  This rule is maintained in case binaries alias UTF-8 strings,
        // which are stored terminated with 0.
        //
        Term_Binary_Len(bin, Binary_Len(bin) + nread);

        if (rebreq->length == UNLIMITED) {
            //
            // Reading an unlimited amount of data, so keep going.
            //
            goto post_read_finished_event;
        }

        assert(rebreq->length >= 0);

        if (rebreq->actual == cast(size_t, rebreq->length)) {
            //
            // We've read as much as we wanted to, so ask to stop reading.
            //

          post_read_finished_event:

            // !!! See note at top of function on why we stop each READ

            rebreq->result = rebBlank();

            // RE: uv_read_stop() "This function will always succeed; hence,
            // checking its return value is unnecessary. A non-zero return
            // indicates that finishing releasing resources may be pending on
            // the next input event on that TTY on Windows, and does not
            // indicate failure."
            //
            uv_read_stop(stream);
            stream->data = nullptr;
        }
        else {
            // Less than the total was reached while reading a limited amount.
            // Don't stop the stream or send an event, keep accruing data.
        }
    }
}


// libuv callback when a write is finished.
//
void on_write_finished(uv_write_t *req, int status)
{
    Reb_Write_Request *rebreq = cast(Reb_Write_Request*, req);

    if (status < 0) {
        rebreq->result = rebError_UV(status);
    }
    else {
        rebreq->result = rebBlank();
    }

    // !!! We could more proactively free memory early for the GC here if
    // we wanted to, presuming we weren't reusing locked data.
    //
    rebRelease(rebreq->binary);
}


//
//  Transport_Actor: C
//
static Bounce Transport_Actor(
    Level* level_,
    REBVAL *port,
    const Symbol* verb,
    enum Transport_Type transport
){
    if (transport == TRANSPORT_UDP)  // disabled for now
        fail ("https://forum.rebol.info/t/fringe-udp-support-archiving/1730");

    Context* ctx = VAL_CONTEXT(port);
    REBVAL *spec = CTX_VAR(ctx, STD_PORT_SPEC);

    // If a transfer is in progress, the port_data is a BINARY!.  Its index
    // represents how much of the transfer has finished.  The data starts
    // as NULL (from `make-port*`) and R3-Alpha would reset it after a
    // transfer was finished.  For writes, R3-Alpha held a copy of the value
    // being written...and text was allowed (even though it might be wide
    // characters, a likely oversight from the addition of unicode).
    //
    REBVAL *port_data = CTX_VAR(ctx, STD_PORT_DATA);
    assert(Is_Binary(port_data) or Is_Nulled(port_data));

    SOCKREQ *sock;
    REBVAL *state = CTX_VAR(ctx, STD_PORT_STATE);
    if (Is_Handle(state)) {
        sock = Sock_Of_Port(port);
        assert(sock->transport == transport);
    }
    else {
        // !!! The Make_Devreq() code would zero out the struct, so to keep
        // things compatible while ripping out the devreq code this must too.
        //
        assert(Is_Nulled(state));
        sock = Try_Alloc(SOCKREQ);
        memset(sock, 0, sizeof(SOCKREQ));
        Init_Handle_Cdata_Managed(
            state,
            sock,
            sizeof(SOCKREQ),
            &cleanup_sockreq
        );
        sock->transport = transport;
        sock->stream = nullptr;

        // !!! There is no way to customize the timeout.  Where should this
        // setting be configured?
    }

    if (sock->stream == nullptr) {
        //
        // Actions for an unopened socket
        //
        switch (Symbol_Id(verb)) {
          case SYM_REFLECT: {
            INCLUDE_PARAMS_OF_REFLECT;

            UNUSED(ARG(value));  // covered by `port`
            Option(SymId) property = Cell_Word_Id(ARG(property));

            switch (property) {
              case SYM_OPEN_Q:
                return Init_False(OUT);

              default:
                break;
            }

            fail (Error_On_Port(SYM_NOT_OPEN, port, -12)); }

          case SYM_OPEN: {
            REBVAL *arg = Obj_Value(spec, STD_PORT_SPEC_NET_HOST);
            REBVAL *port_id = Obj_Value(spec, STD_PORT_SPEC_NET_PORT_ID);

            // OPEN needs to know to bind() the socket to a local port before
            // the first sendto() is called, if the user is particular about
            // what the port ID of originating messages is.  So local_port
            // must be set before the OS_Do_Device() call.
            //
            REBVAL *local_id = Obj_Value(spec, STD_PORT_SPEC_NET_LOCAL_ID);
            if (Is_Nulled(local_id))
                sock->local_port_number = 0;  // let the system pick
            else if (Is_Integer(local_id))
                sock->local_port_number = VAL_INT32(local_id);
            else
                fail ("local-id field of PORT! spec must be NULL or INTEGER!");

            // !!! R3-Alpha would open the socket using `socket()` call, and
            // then do a DNS lookup afterward if necessary.  But the right
            // way to do it is to look up the DNS first and find out what kind
            // of socket to create (e.g. IPv4 vs IPv6, for instance).

            bool listen;
            if (Is_Text(arg)) {
                listen = false;
                sock->remote_port_number =
                    Is_Integer(port_id) ? VAL_INT32(port_id) : 80;

                // Note: sets remote_ip field
                //
                REBVAL *lookup_error = Lookup_Socket_Synchronously(port, arg);
                if (lookup_error)
                    fail (lookup_error);
            }
            else if (Is_Tuple(arg)) {  // Host IP specified:
                listen = false;
                sock->remote_port_number =
                    Is_Integer(port_id) ? VAL_INT32(port_id) : 80;

                Get_Tuple_Bytes(&sock->remote_ip, arg, 4);
            }
            else if (Is_Nulled(arg)) {  // No host, must be a LISTEN socket:
                listen = true;
                sock->local_port_number =
                    Is_Integer(port_id) ? VAL_INT32(port_id) : 8000;
            }
            else
                fail (Error_On_Port(SYM_INVALID_SPEC, port, -10));

            REBVAL *open_error = Open_Socket(port);
            if (open_error)
                fail (open_error);

            if (listen) {
                REBVAL *listen_error = Start_Listening_On_Socket(port);
                if (listen_error)
                    fail (listen_error);
            }

            return COPY(port); }

          case SYM_CLOSE:
            return COPY(port);

          default:
            fail (Error_On_Port(SYM_NOT_OPEN, port, -12));
        }
    }

  //=//// ACTIONS ON "OPEN" SOCKETS ////////////////////////////////////////=//

    switch (Symbol_Id(verb)) { // Ordered by frequency
      case SYM_REFLECT: {
        INCLUDE_PARAMS_OF_REFLECT;

        UNUSED(ARG(value)); // covered by `port`
        Option(SymId) property = Cell_Word_Id(ARG(property));

        switch (property) {
          case SYM_LENGTH: {
            return Init_Integer(
                OUT,
                Is_Binary(port_data) ? Cell_Series_Len_Head(port_data) : 0
            ); }

          case SYM_OPEN_Q:
            //
            // Connect for clients, bind for servers:
            //
            return Init_Logic(
                OUT,
                (sock->modes & RSM_BIND) or (sock->stream != nullptr)
            );

          default:
            break;
        }

        break; }

      case SYM_READ: {
        INCLUDE_PARAMS_OF_READ;

        UNUSED(PARAM(source));

        if (REF(seek))
            fail (Error_Bad_Refines_Raw());

        UNUSED(PARAM(string)); // handled in dispatcher
        UNUSED(PARAM(lines)); // handled in dispatcher

        if (sock->stream == nullptr and sock->transport != TRANSPORT_UDP)
            fail (Error_On_Port(SYM_NOT_CONNECTED, port, -15));

        Reb_Read_Request *rebreq = rebAlloc(Reb_Read_Request);
        rebreq->port_ctx = VAL_CONTEXT(port);
        rebreq->actual = 0;
        rebreq->result = nullptr;

        if (REF(part)) {
            if (not Is_Integer(ARG(part)))
                fail (ARG(part));

            rebreq->length = VAL_INT32(ARG(part));
        }
        else {
            // !!! R3-Alpha didn't have a working READ/PART for networking; it
            // would just accrue data as each chunk came in.  The inability
            // to limit the read length meant it was difficult to implement
            // network protocols.  Ren-C has R3-Alpha's behavior if no /PART
            // is specified.
            //
            rebreq->length = UNLIMITED;  // -1, e.g. "read as much as you can"
        }

        // handle_t* passed to the on_read_alloc callback is the TCP handle
        //
        sock->tcp.data = rebreq;

        int r = uv_read_start(sock->stream, on_read_alloc, on_read);
        if (r < 0)
            return RAISE(rebError_UV(r));  // e.g. "broken pipe" ?

        do {
            uv_run(uv_default_loop(), UV_RUN_ONCE);
        } while (rebreq->result == nullptr);

        if (not Is_Blank(rebreq->result))
            return RAISE(rebreq->result);  // e.g. "broken pipe" ?
        rebRelease(rebreq->result);

        rebFree(rebreq);

        return COPY(port); }

      case SYM_WRITE: {
        INCLUDE_PARAMS_OF_WRITE;

        UNUSED(PARAM(destination));

        if (REF(seek) or REF(append) or REF(lines))
            fail (Error_Bad_Refines_Raw());

        if (sock->stream == nullptr and sock->transport != TRANSPORT_UDP)
            fail (Error_On_Port(SYM_NOT_CONNECTED, port, -15));

        // !!! R3-Alpha did not lay out the invariants of the port model,
        // or what datatypes it would accept at what levels.  TEXT! could be
        // sent here--and it once could be wide characters or Latin1 without
        // the user having knowledge of which.  UTF-8 everywhere has resolved
        // that point (always UTF-8 bytes)...but the port model needs a top
        // to bottom review of what types are accepted where and why.
        //
        REBVAL *data = ARG(data);

        // When we get the callback we'll get the libuv req pointer, which is
        // the same pointer as the rebreq (first struct member).
        //
        Reb_Write_Request *rebreq = rebAlloc(Reb_Write_Request);
        rebreq->port_ctx = VAL_CONTEXT(port);  // API handle for GC safety?
        rebreq->result = nullptr;

        // Make a copy of the BINARY! to put in the request, so that you can
        // say things like:
        //
        //     data: {abc}
        //     write port data
        //     reverse data
        //     write port data
        //
        // We don't want that to be nondeterministic and say {abccba} sometimes
        // and {cbacba} sometimes.  With multithreading it could be worse if
        // the reverse happened in mid-transfer.  :-/
        //
        // We also want to make sure the /PART is handled correctly, so by
        // delegating to COPY/PART we get that for free.
        //
        // !!! If you FREEZE the data then a copy is not necessary, review
        // this as an angle on efficiency.
        //
        rebreq->binary = rebValue(
            "as binary! copy/part", data, rebQ(ARG(part))
        );
        rebUnmanage(rebreq->binary);  // otherwise would be seen as a leak

        uv_buf_t buf;
        buf.base = s_cast(m_cast(Byte*, Cell_Binary_At(rebreq->binary)));
        buf.len = Cell_Series_Len_At(rebreq->binary);
        int r = uv_write(&rebreq->req, sock->stream, &buf, 1, on_write_finished);
        if (r < 0)
            return RAISE(rebError_UV(r));  // e.g. "broken pipe" ?

        do {
            uv_run(uv_default_loop(), UV_RUN_ONCE);
        } while (rebreq->result == nullptr);

        if (not Is_Blank(rebreq->result))
            return RAISE(rebreq->result);  // e.g. "broken pipe" ?
        rebRelease(rebreq->result);

        rebFree(rebreq);

        return COPY(port); }

      case SYM_QUERY: {
        //
        // !!! There are bigger plans for a QUERY dialect (like PARSE).  This
        // old behavior of getting the IP addresses is for legacy only.

        REBVAL *result = rebValue(
            "copy ensure object! (@", port, ").scheme.info"
        );  // shallow copy

        Context* info = VAL_CONTEXT(result);

        Init_Tuple_Bytes(
            CTX_VAR(info, STD_NET_INFO_LOCAL_IP),
            cast(Byte*, &sock->local_ip),
            4
        );
        Init_Integer(
            CTX_VAR(info, STD_NET_INFO_LOCAL_PORT),
            sock->local_port_number
        );

        Init_Tuple_Bytes(
            CTX_VAR(info, STD_NET_INFO_REMOTE_IP),
            cast(Byte*, &sock->remote_ip),
            4
        );
        Init_Integer(
            CTX_VAR(info, STD_NET_INFO_REMOTE_PORT),
            sock->remote_port_number
        );

        return result; }

      case SYM_CLOSE: {
        if (sock->stream) {  // allows close of closed socket (?)
            REBVAL *error = Close_Socket(port);
            if (error)
                fail (error);
        }
        return COPY(port); }

      case SYM_CONNECT: {
        //
        // CONNECT may happen synchronously, or asynchronously...so this may
        // add to Net_Connectors.
        //
        // UDP is connectionless so it will not add to the connectors.
        //
        REBVAL *error = Request_Connect_Socket(port);
        if (error != nullptr)
            fail (error);

        return COPY(port); }

      default:
        break;
    }

    fail (UNHANDLED);
}


//
//  TCP_Actor: C
//
static Bounce TCP_Actor(Level* level_, REBVAL *port, const Symbol* verb)
{
    return Transport_Actor(level_, port, verb, TRANSPORT_TCP);
}


//
//  UDP_Actor: C
//
static Bounce UDP_Actor(Level* level_, REBVAL *port, const Symbol* verb)
{
    return Transport_Actor(level_, port, verb, TRANSPORT_UDP);
}


//
//  export get-tcp-actor-handle: native [
//
//  {Retrieve handle to the native actor for TCP}
//
//      return: [handle!]
//  ]
//
DECLARE_NATIVE(get_tcp_actor_handle)
{
    NETWORK_INCLUDE_PARAMS_OF_GET_TCP_ACTOR_HANDLE;

    Make_Port_Actor_Handle(OUT, &TCP_Actor);
    return OUT;
}


//
//  export get-udp-actor-handle: native [
//
//  {Retrieve handle to the native actor for UDP}
//
//      return: [handle!]
//  ]
//
DECLARE_NATIVE(get_udp_actor_handle)
//
// !!! Note: has not been ported to libuv.
{
    NETWORK_INCLUDE_PARAMS_OF_GET_UDP_ACTOR_HANDLE;

    Make_Port_Actor_Handle(OUT, &UDP_Actor);
    return OUT;
}


uv_timer_t wait_timer;

void wait_timer_callback(uv_timer_t* handle) {
    assert(handle->data != nullptr);
    handle->data = nullptr;
}


uv_timer_t halt_poll_timer;

void halt_poll_timer_callback(uv_timer_t* handle) {
    //
    // Doesn't actually do anything, just breaks the UV_RUN_ONCE loop
    // every half a second.  Theoretically we could do this with uv_signal_t
    // for SIGINT and a callback like:
    //
    //     void signal_callback(uv_signal_t* handle, int signum) {
    //         SET_SIGNAL(SIG_HALT);
    //     }
    //
    // But that seems to only work on Linux and not Windows.
    //
    UNUSED(handle);
}


//
//  startup*: native [  ; Note: DO NOT EXPORT!
//
//  {Initialize Network Extension (e.g. call WSAStartup() on Windows)}
//
//      return: [~]
//  ]
//
DECLARE_NATIVE(startup_p)
//
// Intialize networking libraries and related interfaces.  This needs to be
// called prior to any socket functions.
//
// !!! Note the DNS extension currently relies on this startup being called
// instead of doing its own.
{
    NETWORK_INCLUDE_PARAMS_OF_STARTUP_P;

  #if TO_WINDOWS
    //
    // LibUV calls WSAStartup with MAKEWORD(2, 2) on demand.  Which means that
    // we don't have to on the first startup.  But it never calls WSACleanup(),
    // so we do in SHUTDOWN*.
    //
    // In order to get the number of WSAStartup and WSAShutdown calls to match,
    // we thus need to call startup every time but the first!
    //
    static bool first_startup = true;
    if (first_startup) {
        WSADATA wsaData;
        if (WSAStartup(MAKEWORD(2, 2), &wsaData))
            rebFail_OS (WSAGetLastError());
        first_startup = false;
    }
  #endif

    uv_timer_init(uv_default_loop(), &wait_timer);

    uv_timer_init(uv_default_loop(), &halt_poll_timer);

    return rebTrash();
}


//
//  shutdown*: native [  ; Note: DO NOT EXPORT!
//
//  {Shutdown Network Extension}
//
//      return: [~]
//  ]
//
DECLARE_NATIVE(shutdown_p)
{
    NETWORK_INCLUDE_PARAMS_OF_SHUTDOWN_P;

  #if TO_WINDOWS
    WSACleanup();  // have to call as libuv does not
  #endif

    uv_close(cast(uv_handle_t*, &wait_timer), nullptr);  // no close callback
    uv_close(cast(uv_handle_t*, &halt_poll_timer), nullptr);

    return rebTrash();
}


//
//  export wait*: native [
//
//  "Waits for a duration, port, or both."
//
//      return: "NULL if timeout, PORT! that awoke or BLOCK! of ports if /ALL"
//          [<opt> port! block!]
//      value [<opt> any-number! time! port! block!]
//  ]
//
DECLARE_NATIVE(wait_p)  // See wrapping function WAIT in usermode code
//
// WAIT* expects a BLOCK! argument to have been pre-reduced; this means it
// does not have to implement the reducing process "stacklessly" itself.  The
// stackless nature comes for free by virtue of REDUCE-ing in usermode.
{
    NETWORK_INCLUDE_PARAMS_OF_WAIT_P;

    REBLEN timeout = 0;  // in milliseconds
    REBVAL *ports = nullptr;

    Element(const*) val;
    if (not Is_Block(ARG(value)))
        val = cast(Element(*), ARG(value));
    else {
        ports = ARG(value);

        REBLEN num_pending = 0;
        Element(const*) tail;
        val = Cell_Array_At(&tail, ports);
        for (; val != tail; ++val) {  // find timeout
            if (Is_Port(val))
                ++num_pending;

            if (Is_Integer(val) or Is_Decimal(val) or Is_Time(val))
                break;
        }
        if (val == tail) {
            if (num_pending == 0)
                return nullptr; // has no pending ports!
            timeout = ALL_BITS; // no timeout provided
            val = nullptr;
        }
    }

    if (val != nullptr) {
        switch (VAL_TYPE(val)) {
          case REB_INTEGER:
          case REB_DECIMAL:
          case REB_TIME:
            timeout = Milliseconds_From_Value(val);
            break;

          case REB_PORT: {
            Array* single = Make_Array(1);
            Append_Value(single, val);
            Init_Block(ARG(value), single);
            ports = ARG(value);

            timeout = ALL_BITS;
            break; }

          case REB_BLANK:
            timeout = ALL_BITS; // wait for all windows
            break;

          default:
            fail (Error_Bad_Value(val));
        }
    }

    const uint64_t repeat_ms = 0;  // do not repeat the timer

    if (timeout != ALL_BITS) {
        wait_timer.data = &wait_timer;  // nulled by callback if timeout
        uv_timer_start(&wait_timer, &wait_timer_callback, timeout, repeat_ms);
    }

    // !!! See halt_poll_timer_callback() on why not uv_signal_t for SIGINT
    //
    uv_timer_start(&halt_poll_timer, &halt_poll_timer_callback, 500, 500);

    // Let any pending device I/O have a chance to run.  UV_RUN_ONCE means it
    // will block until *something* happens (could be the timer timing out,
    // or could be something like an incoming network connection being made).
    //
    while (
        (timeout == ALL_BITS or wait_timer.data != nullptr)
        and not GET_SIGNAL(SIG_HALT)
    ){
        int callbacks_left = uv_run(uv_default_loop(), UV_RUN_ONCE);
        UNUSED(callbacks_left);
    }

    uv_timer_stop(&halt_poll_timer);

    if (timeout != ALL_BITS) {
        uv_timer_stop(&wait_timer);
    }

    if (GET_SIGNAL(SIG_HALT)) {
        CLR_SIGNAL(SIG_HALT);

        return Init_Thrown_With_Label(LEVEL, Lib(NULL), Lib(HALT));
    }

    if (GET_SIGNAL(SIG_INTERRUPT)) {
        CLR_SIGNAL(SIG_INTERRUPT);

        // !!! If implemented, this would allow triggering a breakpoint
        // with a keypress.  This needs to be thought out a bit more,
        // but may not involve much more than running `BREAKPOINT`.
        //
        fail ("BREAKPOINT from SIG_INTERRUPT not currently implemented");
    }

    return nullptr;
}
