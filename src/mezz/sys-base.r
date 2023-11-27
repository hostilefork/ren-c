REBOL [
    System: "REBOL [R3] Language Interpreter and Run-time Environment"
    Title: "REBOL 3 Boot Sys: Top Context Functions"
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
        Follows the BASE lib init that provides a basic set of functions
        to be able to evaluate this code.

        The boot binding of this module is SYS then LIB deep.
        Any non-local words not found in those contexts WILL BE
        UNBOUND and will error out at runtime!
    }
]


; If the host wants to know if a script or module is loaded, e.g. to print out
; a message.  (Printing directly from this code would be presumptuous.)
;
; !!! This is not blank but is unset because it is risky to have variables
; meant to hold functions be NULL or BLANK! as they turn into no-ops.
;
script-pre-load-hook: ~

enrescue: :lib.enrescue
lib.enrescue: ~  ; forcing long name of SYS.UTIL.ENRESCUE hints it is dangerous

rescue: func [  ; see also TRAP
    {If evaluation encounters a failure, return it, otherwise NULL}

    return: [<opt> any-value!]
    code [block!]
][
    return match error! enrescue code
]


rescue+: func [  ; see also TRAP+
    {Experimental variation of RESCUE using THENable mechanics}

    return: [<opt> any-value!]
    code [block!]
    <local> result
][
    ; If you return a pure NULL with the desire of triggering ELSE, that does
    ; not allow you to return more values.  This uses a lazy object that will
    ; run a THEN branch on error, but then an ELSE branch with the returned
    ; value on non-error...or decay to a pack with NULL for the error and
    ; the result.
    ;
    if error? result: enrescue code [
        return pack [result null]
    ]

    return isotopic make object! [
        else: branch -> [(heavy unmeta :result) then (:branch)]
        decay: [pack [null unmeta result]]
    ]
]


module: func [
    {Creates a new module}

    return: [module!]
    @product "The result of running the body (~quit~ isotope if it ran QUIT)"
        [any-value!]
    @quitting "If requested and quitting, when true PRODUCT is QUIT's argument"
        [logic?]
    spec "The header block of the module (modified)"
        [<opt> block! object!]
    body "The body of the module (all bindings will be overwritten if block)"
        [text! binary! block!]
    /mixin "Bind body to this additional object before executing"
        [object!]
    /into "Add data to existing MODULE! context (vs making a new one)"
        [module!]
    /file "The filename of the body if it needs transcoding"
        [file! url!]
    /line "The line number of the body if it needs transcoding"
        [integer!]
    <local>
        mod  ; note: overwrites MODULO shorthand in this function
][
    mod: any [
        into
        make module! #  ; !!! currently you can only make module from #
    ]

    ; Turn spec into an OBJECT! if it was a block.  See system.standard.header
    ; for a description of the fields and benefits of using a standard object.
    ;
    if block? spec [
        unbind/deep spec
        spec: construct/with/only spec system.standard.header
    ]

    if spec [  ; Validate the important fields of the header, if there is one
        ;
        ; !!! Historically, the `Name:` and `Type:` fields would tolerate
        ; either a quoted word or plain word.  Now only WORD! is tolerated.
        ;
        for-each [var types] [  ; !!! `in spec` doesn't work here, why not?
            spec.name [<opt> word!]
            spec.type [word!]  ; default is `script` from system.standard.header
            spec.version [<opt> tuple!]
            spec.options [<opt> block!]
        ][
            (match types get var) else [
                fail ["Module" var "must be in" mold types "- not" ^(get var)]
            ]
        ]

        ; Default to having an Exports block in the spec if it's a module.
        ;
        ; !!! Should non-Modules be prohibited from having Exports?  Or just
        ; `Type: Script` be prohibited?  It could be the most flexible answer
        ; is that IMPORT works on anything that has a list of Exports, which
        ; would let people design new kinds like `Type: 'SuperModule`, but
        ; such ideas have not been mapped out.
        ;
        all [
            spec.type = 'Module
            not in spec 'Exports
        ] then [
            append spec spread compose [Exports: (make block! 10)]
        ]

        set-adjunct mod spec
    ]

    ; Interning makes the binding of *all* the words be "attached" in their
    ; binding to the created module.  This process does not create any new
    ; storage space for variables.
    ;
    ; Using the /WHERE option to TRANSCODE asks it to do the interning binding
    ; while it scans, so it's in a single pass.  Hence it's good for higher
    ; level constructs like IMPORT* to only scan the header, but hold off on
    ; turning the body into a BLOCK! of code.  Instead they should pass in
    ; the text or binary to scan at this last minute, when the MAKE MODULE! has
    ; been run and `mod` is available to pass in as the /WHERE.
    ;
    ; !!! Future developments should allow a `Baseline: xxx` in the header,
    ; which specifies modules other than lib to inherit from.
    ;
    if block? body [
        intern* mod body  ; will overwrite any existing bindings in BODY
    ] else [
        body: transcode/where/line/file body mod line file
    ]

    ; We add importing and exporting as specializations of lower-level IMPORT*
    ; and EXPORT* functions.  (Those are still available if you ever want to
    ; specify a "where".)
    ;
    ; If you don't want these added, there could be a refinement to MODULE that
    ; would omit them.  But since MODULE is not that complex to write,
    ; probably better to have such cases use MAKE MODULE! instead of MODULE.
    ;
    append mod 'import
    mod.import: specialize :sys.util.import* [where: mod]

    append mod 'intern
    mod.intern: specialize :intern* [where: mod]

    ; If you DO a file, it doesn't get an EXPORT operation...only modules.
    ;
    append mod 'export
    mod.export: if spec and (spec.type = 'Module) [
        specialize :sys.util.export* [where: mod]
    ] else [
        specialize :fail [reason: [
            {Scripts must be invoked via IMPORT to get EXPORT, not DO:}
            (file else ["<was run as text!/binary!>"])
        ]]
    ]

    if object? mixin [bind body mixin]

    ; We need to catch the quit here, because if we do not we cannot return
    ; a module that we created.  The caller expects to get a module back even
    ; if that module's init code decided to QUIT to end processing prematurely.
    ; (QUIT is not a failure when running scripts.)
    ;
    catch/quit [
        ;
        ; If the body didn't get turned into a block (and is still a BINARY!
        ; or similar) then DO'ing it will cause a confusing infinite recursion.
        ; Good to notice the problem before that.
        ;
        assert [block? body]

        set/any 'product ^ do body  ; can't set variable to none/etc.
        quitting: false
    ]
    then ^arg-to-quit -> [
        quitting: true
        set/any 'product arg-to-quit  ; !!! meta convention?
    ]

    return mod
]


; DO of functions, blocks, paths, and other do-able types is done directly by
; C code in DECLARE_NATIVE(do).  But that code delegates to this Rebol function
; for ANY-STRING! and BINARY! types (presumably because it would be laborious
; to express as C).
;
do*: func [
    {SYS: Called by system for DO on datatypes that require special handling}

    return: "Final evaluative product of code or block"
        [<opt> <void> any-value!]
    source "Files, urls and modules evaluate as scripts, other strings don't"
        [file! url! text! binary! tag! the-word!]
    args "Args passed as system.script.args to a script (normally a string)"
        [<opt> any-value!]
    only "Do not catch quits...propagate them"
        [logic?]
][
    ; For the moment, common features of DO and IMPORT are implemented in the
    ; IMPORT* command.  This includes:
    ;
    ; * Changing the working directory to match the file path (or URL "path")
    ;   of a script, and restoring the prior path on completion of failure
    ;
    ; * Isolating the executed code into a MODULE! so that it doesn't leak
    ;   into the caller's context.
    ;
    ; * Turning @xxx identified script names to DO into a URL by looking it
    ;   up in the modules library.  So `do @chess` and `import @json` use
    ;   the same logic to figure out where that name points.
    ;
    ; * Setting system.script to reflect the executing script during its run,
    ;   and then putting it back when control is returned to the caller.
    ;
    ; * Adding specialized IMPORT, EXPORT, and INTERN definitions that know
    ;   where the importing and exporting and interning needs to be done,
    ;   e.g. into the containing context.  (Lower-level mechanics that want
    ;   to avoid this should use MAKE MODULE! instead for full control.)
    ;
    ; But the actual action taken is different: DO returns the evaluative
    ; product of the script as its primary result, while IMPORT returns the
    ; module context the script was executed in.  And IMPORT is designed to
    ; return the same context every time it is called--so modules are loaded
    ; only once--while DO performs an action that you can run any number
    ; of times.  So what DO does is effectively flips the order of the
    ; return results of IMPORT.
    ;
    ; !!! Because DO presumably wants to be able to return stable isotopes,
    ; this is likely done backwards, as having product as a secondary result
    ; means that it has to be meta.
    ;
    return unmeta [_ @]: import*/args/only null source args only
]
