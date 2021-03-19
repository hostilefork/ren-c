REBOL [
    System: "REBOL [R3] Language Interpreter and Run-time Environment"
    Title: "Create C .inc file with const data of r3.exe startup code"
    File: %prep-main.reb
    Rights: {
        Copyright 2012-2021 Ren-C Open Source Contributors
        Copyright 2012 REBOL Technologies
        REBOL is a trademark of REBOL Technologies
    }
    License: {
        Licensed under the Apache License, Version 2.0
        See: http://www.apache.org/licenses/LICENSE-2.0
    }
    Purpose: {
        This compresses together several pieces of Rebol code (with comments
        stripped out), and then turns it into a static C constant byte array.
        It is decompressed by the code in %main.c, which then executes it.

        The process uses much of the same code that embeds the mezzanine into
        the core library.  Notably, it makes very light use of the bootstrap
        Rebol's LOAD function.  Hence code can use arbitrarily modern syntax
        even with an older Rebol being used for the build.
    }
]

do %../../tools/common.r  ; for PARSE-ARGS, STRIPLOAD, BINARY-TO-C...
do %../../tools/common-emitter.r  ; for splicing Rebol into templated strings

args: parse-args system/script/args  ; either from command line or DO/ARGS
output-dir: make-file [(system/options/path) prep /]
mkdir/deep make-file [(output-dir) main /]


buf: make text! 200000


; In order to get the whole process rolling for the r3.exe, it needs to do
; command line processing and read any encapped code out of the executable.
; It would likely want to build on ZIP files to do this, so the unzip script
; is embedded as well.
;
for-each file [
    %../../scripts/unzip.reb
    %../../scripts/encap.reb

    %../../scripts/make-file.r  ; Work in progress for FILE! conversion
    %../../scripts/shell.r  ; SHELL dialect (requires CALL, here for editing)

    ; %prot-http.r and %prot-tls.r don't have any native code directly in them,
    ; so they don't need to be "extensions".  They could just be encapped
    ; (unlike the encap and compression code itself.)  But they represnt
    ; another level of "bootstrap" for those who want to run scripts off the
    ; web, so they make sense to build into the executable.
    ;
    ; Note that these depend on the crypto extension being initialized to run.

    %../../scripts/prot-tls.r  ; TLS (a.k.a. the "S" in HTTPS)
    %../../scripts/prot-http.r  ; HTTP Client (HTTPS if used with TLS)

    %main-startup.reb
][
    print ["Emulating LOAD of header for:" file]

    header: _  ; !!! Was a SET-WORD!...for locals gathering?
    contents: stripload/header file 'header

    is-module: true  ; Everything *should* be a module
    if is-module [
        append/line buf "import module ["
        append/line buf header
        append/line buf "]["
        append/line buf contents
        append/line buf "]"
    ] else [
        append/line buf contents
    ]
]

; The code evaluates to the MAIN-STARTUP function, so it is easily found
; as the result of running the code in %main.c - make it the last line.
;
; (This organization lets us separate the moment of loading from the moment
; of running, in case that were interesting.)
;
append/line buf ":main-startup"


; It's helpful to have an uncompressed readable copy of the bundled and
; stripped init code for inspection.
;
write-if-changed make-file [(output-dir) main /tmp-main-startup.r] buf


(e: make-emitter
    "r3 console executable embedded Rebol code bundle"
    make-file [(output-dir) main/tmp-main-startup.inc])

compressed: gzip buf

e/emit 'compressed {
    /*
     * Gzip compression of host initialization code
     * Originally $<length of buf> bytes
     */
    #define MAIN_STARTUP_SIZE $<length of compressed>
    const unsigned char Main_Startup_Code[MAIN_STARTUP_SIZE] = {
        $<Binary-To-C Compressed>
    };
}

e/write-emitted
