REBOL [
    System: "REBOL [R3] Language Interpreter and Run-time Environment"
    Title: "REBOL 3 Boot Sys: Port and Scheme Functions"
    Rights: {
        Copyright 2012 REBOL Technologies
        REBOL is a trademark of REBOL Technologies
    }
    License: {
        Licensed under the Apache License, Version 2.0
        See: http://www.apache.org/licenses/LICENSE-2.0
    }
    Context: sys
    Note: {
        The boot binding of this module is SYS then LIB deep.
        Any non-local words not found in those contexts WILL BE
        UNBOUND and will error out at runtime!
    }
]

; !!! UPARSE is not available in SYS because it is higher level.  We hack it
; up so that when %uparse.reb runs it pokes itself into sys.util.parse
;
parse: ~sys-util-parse-not-set-yet~

make-port*: function [
    "SYS: Called by system on MAKE of PORT! port from a scheme."

    return: [port!]
    spec [file! url! block! object! word! port!]
        "port specification"
][
    ; The first job is to identify the scheme specified:

    switch/type spec [
        file! [
            name: either (dir? spec) 'dir 'file
            spec: make object! [
                scheme: name
                ref: spec
            ]
        ]
        url! [
            spec: decode-url spec
            name: spec.scheme
        ]
        block! [
            ;
            ; !!! This BLOCK! may be dialected, for instance httpd.reb creates
            ; a server with:
            ;
            ;     make port! [scheme: 'httpd 8000 [render "Hello"]]
            ;
            ; It expects to find that block in `spec.ref`.  But other protocols
            ; are "well formed objects" that expect MAKE OBJECT!, e.g. TLS:
            ;
            ;    make port! [
            ;        scheme: 'tls
            ;        host: port.spec.host
            ;        port-id: port.spec.port-id
            ;        ref: join tcp:// spread reduce [host ":" port-id]
            ;    ]
            ;
            ; So it's a bit weird, being a block that has to survive a call
            ; to MAKE OBJECT! but then also being preserved for dialected
            ; analysis.  All of this needs to be rewritten, but go along with
            ; the strange historical behavior for now.
            ;
            spec: make object! append copy spec spread :['ref: spec]
            name: spec.scheme
        ]
        object! [
            name: get in spec 'scheme
        ]
        word! [
            name: spec
            spec: null
        ]
        port! [
            name: port.scheme.name
            spec: port.spec
        ]

        fail
    ]

    ; Get the scheme definition:
    all [
        word? name
        scheme: get try in system.schemes name
    ] else [
        cause-error 'access 'no-scheme name
    ]

    ; Create the port with the correct scheme spec.
    ;
    ; !!! This used to use MAKE OBJECT! so it got a copy, but now that we are
    ; doing some hacky inheritance manually from object-to-object, there needs
    ; to be a COPY made.
    ;
    port: make system.standard.port []
    port.spec: copy any [scheme.spec, system.standard.port-spec-head]

    ; !!! Override any of the fields in port.spec with fields in spec.
    ; This used to be done with plain object derivation, because spec was
    ; a BLOCK!.  But DECODE-URL now returns an object, and you can't make
    ; an derived object via an object at this time.  Do it manually.
    ;
    ; !!! COLLECT is not available here.  This is all very old stuff and was
    ; organized terribly.  :-(
    ;
    overloads: copy []
    for-each [key val] spec [
        if not any [quasi? ^val, null? :val, blank? :val] [
            append overloads spread :[to set-word! key get 'val]  ; override
        ]
    ]
    append port.spec spread overloads

    port.spec.scheme: name
    port.scheme: scheme

    ; Defaults:
    port.actor: try get in scheme 'actor ; avoid evaluation
    port.spec.ref: default [spec]
    port.spec.title: default [scheme.title]
    port: to port! port

    ; Call the scheme-specific port init. Note that if the
    ; scheme has not yet been initialized, it can be done
    ; at this time.
    if in scheme 'init [scheme.init port]
    return port
]

*parse-url: make object! [
    digit:       make bitset! "0123456789"
    digits:      [repeat ([1 5]) digit]  ; 1 to 5 digits
    alpha-num:   make bitset! [#"a" - #"z" #"A" - #"Z" #"0" - #"9"]
    scheme-char: insert copy alpha-num "+-."
    path-char:   complement make bitset! "#"
    user-char:   complement make bitset! ":@"
    host-char:   complement make bitset! ":/?"

    rules: [
        ; Required scheme name, but "//" is optional (without it is a "URN")
        ; https://en.wikipedia.org/wiki/Uniform_Resource_Name
        ;
        emit scheme: [as/ (word!) across some scheme-char] ":" try "//"

        ; optional user [:pass] @
        [
            emit user: across some user-char
            emit pass: try [":", across to "@"]
            "@"
            |
            emit user: (null)
            emit pass: (~no-user~)  ; is this better than NULL?
        ]

        ; optional host [:port]
        ;
        ; Note: Historically this code tried to detect if the host was like
        ; an IP address, and if so return it as a TUPLE! instead of a TEXT!.
        ; This is used to cue IP address lookup behavior vs. a DNS lookup.
        ; A basis for believing you can discern comes from RFC-1738:
        ;
        ;    "The rightmost domain label will never start with a
        ;     digit, though, which syntactically distinguishes all
        ;     domain names from the IP addresses."
        [
            emit host: [
                ; IP-address style, make a TUPLE!
                ;
                to/ (tuple!) across [
                    try some [some digit "."], some digit
                    not host-char  ; don't match "1.2.3.4a" as IP address
                ]
                    |
                ; Ordinary "foo.bar.com" style, just give it back as TEXT!
                ;
                across some host-char
                    |
                ; Missing host, set as null
                ;
                ahead [":" | <end>] (null)
            ]
            emit port-id: try [":", to/ (integer!) across digits]
        ]

        emit path: try [across some path-char]  ; optional path

        emit tag: try ["#", across to <end>]  ; optional bookmark ("tag")

        emit ref: as/ (url!) <input>  ; alway save original URL for reference
    ]

    ; !!! Historically DECODE-URL returned a BLOCK!, but an object seems
    ; better.  Also, it seems useful to have NULL versions of the fields
    ; even for objects that don't have them to make it easy to check if those
    ; fields are present.
    ;
    ; !!! Red takes a similar approach of returning an object with a fixed
    ; list of fields but breaks them down differently and uses different names.
    ; That should be reviewed.
    ;
    decode-url: func [  ; this function is bound in sys.util.*parse-url
        {Decode a URL according to rules of sys.util.*parse-url}
        return: [object!]
        url [url! text!]
    ][
        return parse as text! url [gather rules] except [
            fail ["Could not decode URL to an object:" url]
        ]
    ]
]

decode-url: :*parse-url.decode-url  ; wrapped in context, expose function

;-- Native Schemes -----------------------------------------------------------

make-scheme: function [
    {Make a scheme from a specification and add it to the system}

    return: [~]
    def "Scheme specification"
        [block!]
    /with "Scheme name to use as base"
        [word!]
][
    with: either with [get in system.schemes with] [system.standard.scheme]
    if not with [cause-error 'access 'no-scheme with]

    scheme: make with def
    if not scheme.name [cause-error 'access 'no-scheme-name scheme]

    ; If actor is block build a non-contextual actor object:
    if block? scheme.actor [
        actor: make object! (length of scheme.actor) / 4
        for-each [name op args body] scheme.actor [
            assert [
                set-word? name
                find [func lambda function] op  ; why'd R3-Alpha constrain this?
                block? args
                block? body
            ]
            append actor name: to word! name  ; !!! use EXTEND-style operation?
            actor.(name): reeval op args body ; add action to object! w/name
        ]
        scheme.actor: actor
    ]

    match [object! handle!] scheme.actor else [
        fail ["Scheme actor" scheme.name "can't be" kind of scheme.actor]
    ]

    append system.schemes spread reduce [scheme.name scheme]
]
