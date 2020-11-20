REBOL []

name: 'Debugger
source: %debugger/mod-debugger.c
includes: reduce [
    %prep/extensions/debugger/ ;for %tmp-extensions-debugger-init.inc

    make-file [(repo-dir) extensions/debugger /]
    make-file [(repo-dir) extensions/debugger/libwebsockets/include /]

    make-file [(repo-dir) extensions/debugger/libwebsockets/lib/core /]

    make-file [(repo-dir) extensions/debugger/libwebsockets/lib/plat/unix /]
    make-file [(repo-dir) extensions/debugger/libwebsockets/lib/event-libs /]
    make-file [(repo-dir) extensions/debugger/libwebsockets/lib/core-net /]

    make-file [(repo-dir) extensions/debugger/libwebsockets/lib/roles /]
    make-file [(repo-dir) extensions/debugger/libwebsockets/lib/roles/http /]
    make-file [(repo-dir) extensions/debugger/libwebsockets/lib/roles/h1 /]
    make-file [(repo-dir) extensions/debugger/libwebsockets/lib/roles/ws /]

    ; This appears to *require* pthreds.  can it be avoided?
    ;make-file [(repo-dir) extensions/debugger/libwebsockets/lib/system/smd /]
]
definitions: [
    {MBEDTLS_CONFIG_FILE="mbedtls-rebol-config.h"}
]
depends: compose/deep [
    (elide ws-opts: [
        #no-c++
        <gnu:-Wno-redundant-decls>
        <gnu:-Wno-unused-parameter>
        <gnu:-Wno-unused-variable>
    ])

    ; ACTUAL FILES USED

    %debugger/ws-server.c
    %debugger/ws-client.c

    ; FILES FOR LIBWEBSOCKETS
    ; An attempt has been made to get this down to just the files that are
    ; required to build the echo server and echo client for websockets.
    ; This winds up pulling in quite a bit; because websockets fundamentally
    ; build on top of HTTP.

    ; %core/ files
    [%debugger/libwebsockets/lib/core/alloc.c  (ws-opts)]
    [%debugger/libwebsockets/lib/core/buflist.c  (ws-opts)]
    [%debugger/libwebsockets/lib/core/context.c  (ws-opts)]
    [
        %debugger/libwebsockets/lib/core/libwebsockets.c  (ws-opts)
        <gnu:-Wno-discarded-qualifiers>
    ]
    [%debugger/libwebsockets/lib/core/logs.c  (ws-opts)]
    [%debugger/libwebsockets/lib/core/lws_dll2.c  (ws-opts)]  ; used by %core-net/service.c and many other %core-net files
    ; [%debugger/libwebsockets/lib/core/vfs.c  (ws-opts)]

    [%debugger/libwebsockets/lib/system/system.c  (ws-opts)]

    ; These hashes are needed by e.g. lws_generate_client_ws_handshake
    ;
    [%debugger/libwebsockets/lib/misc/base64-decode.c  (ws-opts)]
    [%debugger/libwebsockets/lib/misc/sha-1.c  (ws-opts)]

    ; This doesn't seem to be used by the library proper, but it is used by
    ; the `protocol_lws_server.c`
    ; https://libwebsockets.org/lws-api-doc-master/html/group__lws__ring.html
    [%debugger/libwebsockets/lib/misc/lws-ring.c  (ws-opts)]

    ; === %roles (/http /h1 /ws)
    [
        %debugger/libwebsockets/lib/roles/h1/ops-h1.c  (ws-opts)
        <gnu:-Wno-discarded-qualifiers>
    ]

    [%debugger/libwebsockets/lib/roles/ws/client-parser-ws.c  (ws-opts)]
    [%debugger/libwebsockets/lib/roles/ws/client-ws.c  (ws-opts)]
    [%debugger/libwebsockets/lib/roles/ws/ops-ws.c  (ws-opts)]
    [%debugger/libwebsockets/lib/roles/ws/server-ws.c  (ws-opts)]

    ; [%debugger/libwebsockets/client    server      compression

    [%debugger/libwebsockets/lib/roles/http/header.c  (ws-opts)]
    [%debugger/libwebsockets/lib/roles/http/parsers.c  (ws-opts)]
    [%debugger/libwebsockets/lib/roles/http/date.c  (ws-opts)]
    [%debugger/libwebsockets/lib/roles/http/client/client-http.c  (ws-opts)]
    [%debugger/libwebsockets/lib/roles/http/server/server.c  (ws-opts)]

    [
        %debugger/libwebsockets/lib/roles/listen/ops-listen.c  (ws-opts)
        <gnu:-Wno-logical-op>
    ]

    [%debugger/libwebsockets/lib/roles/pipe/ops-pipe.c  (ws-opts)]

    [%debugger/libwebsockets/lib/roles/raw-skt/ops-raw-skt.c  (ws-opts)]


    [%debugger/libwebsockets/lib/plat/unix/unix-fds.c  (ws-opts)]  ; needed for wsi_from_fd
    [%debugger/libwebsockets/lib/plat/unix/unix-init.c  (ws-opts)]
    [%debugger/libwebsockets/lib/plat/unix/unix-pipe.c  (ws-opts)]
    ; [%debugger/libwebsockets/lib/plat/unix/unix-resolv.c  (ws-opts)]
    [%debugger/libwebsockets/lib/plat/unix/unix-sockets.c  (ws-opts)]
    [%debugger/libwebsockets/lib/plat/unix/unix-caps.c  (ws-opts)]  ; lws_plat_drop_app_privileges() used by libwebsockets.c, context.c
    ; [%debugger/libwebsockets/lib/plat/unix/unix-file.c  (ws-opts)]
    [%debugger/libwebsockets/lib/plat/unix/unix-misc.c  (ws-opts)]  ; lws_now_usecs()
    ; [%debugger/libwebsockets/lib/plat/unix/unix-plugins.c  (ws-opts)]
    [%debugger/libwebsockets/lib/plat/unix/unix-service.c  (ws-opts)]
    ; [%debugger/libwebsockets/lib/plat/unix/unix-spawn.c  (ws-opts)]

    [%debugger/libwebsockets/lib/event-libs/poll/poll.c  (ws-opts)]

    ; %core-net/
    [%debugger/libwebsockets/lib/core-net/adopt.c  (ws-opts)]
    [%debugger/libwebsockets/lib/core-net/client/connect.c  (ws-opts)]
    [%debugger/libwebsockets/lib/core-net/client/connect2.c  (ws-opts)]
    [%debugger/libwebsockets/lib/core-net/client/connect3.c  (ws-opts)]
    [%debugger/libwebsockets/lib/core-net/client/connect4.c  (ws-opts)]
    [%debugger/libwebsockets/lib/core-net/client/sort-dns.c  (ws-opts)]
    [
        %debugger/libwebsockets/lib/core-net/close.c  (ws-opts)
        <gnu:-Wno-discarded-qualifiers>
    ]
    [%debugger/libwebsockets/lib/core-net/dummy-callback.c  (ws-opts)]
    [%debugger/libwebsockets/lib/core-net/network.c  (ws-opts)]
    [
        %debugger/libwebsockets/lib/core-net/output.c  (ws-opts)
        <gnu:-Wno-logical-op>
    ]
    [%debugger/libwebsockets/lib/core-net/pollfd.c  (ws-opts)]  ; used by wsi.c
    [%debugger/libwebsockets/lib/core-net/service.c  (ws-opts)]
    [%debugger/libwebsockets/lib/core-net/sorted-usec-list.c  (ws-opts)]
    [%debugger/libwebsockets/lib/core-net/state.c  (ws-opts)]

    [%debugger/libwebsockets/lib/core-net/vhost.c  (ws-opts)]  ; lws_vhost_unbind_wsi(), needed by wsi.c
    [%debugger/libwebsockets/lib/core-net/wsi.c  (ws-opts)]  ; central, lwsi_set_state() needed by %roles/ws/ops-ws.c
    [%debugger/libwebsockets/lib/core-net/wsi-timeout.c  (ws-opts)]
]

; !!! The debugger spawns nested console sessions, with some customization.
; It might be desirable to be able to build without a specific idea of what
; the DEBUG-CONSOLE command does.  But for now it's an ADAPT of the CONSOLE
; function with the interface as defined in the console extension, and to
; make that adaptation at module initialization time the function must be
; already loaded in the sequence.
;
requires: 'Console
