REBOL [
    System: "REBOL [R3] Language Interpreter and Run-time Environment"
    Title: "REBOL 3 Boot Base: Constants and Equates"
    Rights: {
        Copyright 2012 REBOL Technologies
        REBOL is a trademark of REBOL Technologies
    }
    License: {
        Licensed under the Apache License, Version 2.0
        See: http://www.apache.org/licenses/LICENSE-2.0
    }
    Note: {
        This code is evaluated just after actions, natives, sysobj, and other lower
        levels definitions. This file intializes a minimal working environment
        that is used for the rest of the boot.
    }
]

; NOTE: The system is not fully booted at this point, so only simple
; expressions are allowed. Anything else will crash the boot.

; Standard constants

on:  true
off: false
yes: true
no:  false
zero: 0

; Special values

lib: system.contexts.lib

; Char constants

nul: NUL:  #"^(NULL)"
space:     #" "
sp: SP:    space
backspace: #"^(BACK)"
bs: BS:    backspace
tab:       #"^-"
newline:   #"^/"
newpage:   #"^l"
slash:     #"/"
backslash: #"\"
escape:    #"^(ESC)"
cr: CR:    #"^M"
lf: LF:    newline

; Type predicates.  These are planned to all go away in favor of plain &
; decorations (e.g. &any-series?) when needed.  Use e.g. plain any-series?
; in function specs.

null?!: &null?
trash?!: &trash?
void?!: &void?
char?!: &char?
blackhole?!: &blackhole?
splice?!: &splice?
logic?!: &logic?
action?!: &action?


; A "blackhole" is a name for the usage of the NUL character in the sense of
; "truthy emptiness".  e.g. `set # 10` will not error, while `set _ 10` will.
; It was chosen for its looks, and since it also acts as the '\0' character it
; feels a bit like "sending things to `/dev/null`".
;
blackhole: #
void: '
