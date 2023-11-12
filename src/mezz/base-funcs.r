REBOL [
    System: "REBOL [R3] Language Interpreter and Run-time Environment"
    Title: "REBOL 3 Boot Base: Function Constructors"
    Rights: {
        Copyright 2012 REBOL Technologies
        Copyright 2012-2019 Ren-C Open Source Contributors
        REBOL is a trademark of REBOL Technologies
    }
    License: {
        Licensed under the Apache License, Version 2.0
        See: http://www.apache.org/licenses/LICENSE-2.0
    }
    Note: {
        This code is evaluated just after actions, natives, sysobj, and other
        lower-level definitions.  It intializes a minimal working environment
        that is used for the rest of the boot.
    }
]

assert: func* [
    {Ensure conditions are conditionally true if hooked by debugging}

    return: <void>
    conditions "Block of conditions to evaluate and test for logical truth"
        [block!]
    /handler "Optional code to run if the assertion fails, receives condition"
        [<unrun> block! frame!]
][
    ; ASSERT has no default implementation, but can be HIJACKed by a debug
    ; mode with a custom validation or output routine.
    ;
    ; !!! R3-Alpha and Rebol2 did not distinguish ASSERT and VERIFY, since
    ; there was no idea of a "debug mode"
]

steal: lambda [
    {Return a variable's value prior to an assignment, then do the assignment}

    evaluation "Takes assigned value (variadic enables STEAL X: DEFAULT [...])"
        [<opt> any-value! <variadic>]
    'look [set-word! set-tuple! <variadic>]
][
    get first look  ; returned value
    elide take evaluation
]

assert [null = binding of :return]  ; it's archetypal, nowhere to return to
return: func* [] [
    fail "RETURN archetype called when no generator is providing it"
]

func: func* [
    {Make action with set-words as locals, <static>, <in>, <with>, <local>}

    return: [activation!]
    spec "Help string (opt) followed by arg words (and opt type and string)"
        [block!]
    body "The body block of the function"
        [<const> block!]
    /gather "Gather SET-WORD! as local variables (preferably, please use LET)"
    <local>
        new-spec var loc other
        new-body exclusions locals defaulters statics with-return
][
    ; R3-Alpha offered features on FUNCTION (a complex usermode construct)
    ; that the simpler/faster FUNC did not have.  Ren-C seeks to make FUNC and
    ; FUNCTION synonyms:
    ;
    ; https://forum.rebol.info/t/abbreviations-as-synonyms/1211
    ;
    ; To get a little ways along this path, there needs to be a way for FUNC
    ; to get features like <static> which are easier to write in usermode.
    ; So the lower-level FUNC* is implemented as a native, and this wrapper
    ; does a fast shortcut to check to see if the spec has no tags...and if
    ; not, it quickly falls through to that fast implementation.
    ;
    ; Note: Long term, FUNC could be a native which does this check in raw
    ; C and then calls out to usermode if there are tags.  That would be even
    ; faster than this usermode prelude.
    ;
    all [
        not gather
        not find spec matches tag!
        return func* spec body
    ]

    exclusions: copy []

    ; Rather than MAKE BLOCK! LENGTH OF SPEC here, we copy the spec and clear
    ; it.  This costs slightly more, but it means we inherit the file and line
    ; number of the original spec...so when we pass NEW-SPEC to FUNC or PROC
    ; it uses that to give the FILE OF and LINE OF the function itself.
    ;
    ; !!! General API control to set the file and line on blocks is another
    ; possibility, but since it's so new, we'd rather get experience first.
    ;
    new-spec: clear copy spec

    new-body: null
    statics: null
    defaulters: null
    var: #dummy  ; enter PARSE with truthy state (gets overwritten)
    loc: null
    with-return: null

    parse3 spec [try some [
        '<none> (append new-spec <none>)
    |
        '<void> (append new-spec <void>)
    |
        :(if var '[  ; so long as we haven't reached any <local> or <with> etc.
            set var: [any-word! | any-path! | quoted!] (
                append new-spec var

                var: noquote var
                case [
                    match [get-path! path!] var [
                        if (length of var != 2) or ('_ <> var.1) [
                            fail ["Bad path in spec:" ^spec]
                        ]
                        append exclusions var.2  ; exclude args/refines
                    ]

                    any-word? var [
                        append exclusions var  ; exclude args/refines
                    ]

                    true [  ; QUOTED! could have been anything
                        fail ["Bad spec item" ^var]
                    ]
                ]
            )
            |
            set other: block! (
                append new-spec other  ; data type blocks
            )
            |
            copy other some text! (
                append new-spec spaced other  ; spec notes
            )
        ] else [
            [false]
        ])
    |
        other: <here>
        group! (
            if not var [
                fail [
                    ; <where> spec
                    ; <near> other
                    "Default value not paired with argument:" (mold other.1)
                ]
            ]
            defaulters: default [copy []]
            append defaulters spread compose [
                (var): default '(eval other.1)  ; !!! meta?
            ]
        )
    |
        (var: null)  ; everything below this line resets var
        false  ; failing here means rolling over to next rule
    |
        '<local> (append new-spec <local>)
        try some [set var: word! set other: try group! (
            append new-spec var
            append exclusions var
            if other [
                defaulters: default [copy []]
                append defaulters spread compose [  ; always sets
                    (var): (meta eval other)
                ]
            ]
        )]
        (var: null)  ; don't consider further GROUP!s or variables
    |
        '<in> (
            new-body: default [
                copy/deep body
            ]
        )
        try some [
            set other: [object! | word! | tuple!] (
                if not object? other [other: ensure any-context! get other]
                bind new-body other
                for-each [key val] other [
                    append exclusions key
                ]
            )
        ]
    |
        '<with> try some [
            set other: [word! | path!] (
                append exclusions other

                ; Definitional returns need to be signaled even if FUNC, so
                ; the FUNC* doesn't automatically generate one.
                ;
                if other = 'return [with-return: [<with> return]]
            )
        |
            text!  ; skip over as commentary
        ]
    |
        '<static> (
            statics: default [copy []]
            new-body: default [
                copy/deep body
            ]
        )
        try some [
            set var: word! (other: null) try set other: group! (
                append exclusions var
                append statics (as set-word! var)
                append statics (other else [the ~])
            )
        ]
        (var: null)
    |
        <end> accept (true)
    |
        other: <here> (
            fail [
                ; <where> spec
                ; <near> other
                "Invalid spec item:" (mold ^other.1)
                "in spec" ^spec
            ]
        )
    ]]

    ; Gather the SET-WORD!s in the body, excluding the collected ANY-WORD!s
    ; that should not be considered.  Note that COLLECT is not defined by
    ; this point in the bootstrap.
    ;
    locals: if gather [
        collect-words/deep/set/ignore body exclusions
    ] else [null]

    if statics [
        statics: make object! statics
        bind new-body statics
    ]

    ; The <local> designation may be redundant with a <local> in the original
    ; spec.  Historical implementations of things like FUNCT in R3-Alpha had
    ; to dodge this to keep from providing redundant refinements.  It's legal
    ; in Ren-C, but really this generator should be making a FRAME! and not
    ; a spec block.  This is a work in progress.
    ;
    if locals [
        append new-spec <local>
        append new-spec spread locals
    ]

    append new-spec maybe with-return  ; if FUNC* suppresses return generation

    ; The constness of the body parameter influences whether FUNC* will allow
    ; mutations of the created function body or not.  It's disallowed by
    ; default, but TWEAK can be used to create variations e.g. a compatible
    ; implementation with Rebol2's FUNC.
    ;
    if const? body [new-body: const new-body]

    return func* new-spec either defaulters [
        append defaulters as group! any [new-body body]
    ][
        any [new-body body]
    ]
]


; Historical FUNCTION is intended to one day be a synonym for FUNC, once there
; are solutions such that LET can take the place of what SET-WORD! gathering
; was able to do.  This will be an ongoing process.
;
; https://forum.rebol.info/t/rethinking-auto-gathered-set-word-locals/1150
;
function: specialize :func [gather: #]


; Simple "divider-style" thing for remarks.  At a certain verbosity level,
; it could dump those remarks out...perhaps based on how many == there are.
; (This is a good reason for retaking ==, as that looks like a divider.)
;
===: func [
    return: <void>
    'remarks [any-value! <variadic>]
    /visibility [logic!]
    <static> showing (false)
][
    if not null? visibility [showing: visibility, return]

    if showing [
        print form collect [
            keep [===]
            until [equal? ''=== keep take remarks]  ; prints tail `===`
        ]
    ] else [
        until [equal? '=== take remarks]
    ]
]

what-dir: func [  ; This can be HIJACK'd by a "smarter" version
    {Returns the current directory path}
    return: [<opt> file! url!]
][
    return system.options.current-path
]

change-dir: func [  ; This can be HIJACK'd by a "smarter" version
    {Changes the current path (where scripts with relative paths will be run).}
    return: [file! url!]
    path [file! url!]
][
    return system.options.current-path: path
]


redescribe: func [
    {Mutate action description with new title and/or new argument notes.}

    return: [activation!]
        {The input action, with its description now updated.}
    spec [block!]
        {Either a string description, or a spec block (without types).}
    action [<unrun> frame!]
        {(modified) Action whose description is to be updated.}
][
    let adjunct: adjunct-of action
    let notes: null
    let description: null

    ; For efficiency, objects are only created on demand by hitting the
    ; required point in the PARSE.  Hence `redescribe [] :foo` will not tamper
    ; with the meta information at all, while `redescribe [{stuff}] :foo` will
    ; only manipulate the description.

    let on-demand-adjunct: does [
        adjunct: default [
            set-adjunct action copy system.standard.action-adjunct
        ]

        if not in adjunct 'description [
            fail [{archetype ADJUNCT OF doesn't have DESCRIPTION slot} meta]
        ]

        if notes: select adjunct 'parameter-notes [
            if not any-context? notes [  ; !!! Sometimes OBJECT!, else FRAME!
                fail [{PARAMETER-NOTES in ADJUNCT-OF is not a FRAME!} notes]
            ]

;            all [frame? notes, action <> (action of notes)] then [
;                fail [{PARAMETER-NOTES in ADJUNCT-OF frame mismatch} notes]
;            ]
        ]
    ]

    let on-demand-notes: does [  ; was DOES CATCH, removed during DOES tweak
        on-demand-adjunct

        if find adjunct 'parameter-notes [
            adjunct: null  ; need to get a parameter-notes field in the OBJECT!
            on-demand-adjunct  ; ...so this loses SPECIALIZEE, etc.

            description: adjunct.description: fields.description
            notes: adjunct.parameter-notes: fields.parameter-notes
            types: adjunct.parameter-types: fields.parameter-types
        ]
    ]

    let param: null
    let note: null
    parse3 spec [
        try [
            copy description some text! (
                description: spaced description
                all [
                    description = {}
                    not adjunct
                ] then [
                    ; No action needed (no adjunct to delete old description in)
                ] else [
                    on-demand-adjunct
                    adjunct.description: if description = {} [
                        _
                    ] else [
                        description
                    ]
                ]
            )
        ]
        try some [
            set param: [
                word! | get-word! | lit-word! | set-word!
                | ahead path! into [word! blank!]
            ](
                if path? param [param: param.1]
            )

            ; It's legal for the redescribe to name a parameter just to
            ; show it's there for descriptive purposes without adding notes.
            ; But if {} is given as the notes, that's seen as a request
            ; to delete a note.
            ;
            try [[copy note some text!] (
                note: spaced note
                on-demand-adjunct
                if param = 'return: [
                    adjunct.return-note: all [
                        note <> {}
                        copy note
                    ]
                ] else [
                    any [
                        notes
                        note <> {}
                    ] then [
                        on-demand-notes

                        if not find notes as word! param [
                            fail [param "not found in frame to describe"]
                        ]

                        let actual: first find parameters of :value param
                        if param !== actual [
                            fail [param {doesn't match word type of} actual]
                        ]

                        notes.(as word! param): if note = {} [~] else [note]
                    ]
                ]
            )]
        ]
        <end>
    ] except [
        fail [{REDESCRIBE specs should be STRING! and ANY-WORD! only:} spec]
    ]

    ; If you kill all the notes then they will be cleaned up.  The adjunct
    ; object will be left behind, however.
    ;
    all [
        notes
        every [param note] notes [none? :note]
    ] then [
        adjunct.parameter-notes: null
    ]

    return runs action  ; should have updated the adjunct
]


redescribe [
    {Create an ACTION, implicity gathering SET-WORD!s as <local> by default}
] :function


unset: redescribe [
    {Clear the value of a word to the unset state (in its current context)}
](
    specialize :set [value: meta ~]  ; SET's value is a ^META parameter
)

unset?: func [
    {Determine if a variable looks up to a `~` isotope}
    return: [logic?]
    var [word! path! tuple!]
][
    return none? get/any var
]

set?: func [
    {Determine if a variable does not look up to  `~` isotope}
    return: [logic?]
    var [word! path! tuple!]
][
    return not none? get/any var
]


defined?: func [
    {Determine if a variable is both "attached", and not unset}
    return: [logic?]
    var [word! path! tuple!]
][
    if tuple? var [
        return not trap [get var]  ; can't use BINDING OF on TUPLE! atm
    ]
    return did all [
        '~attached~ != binding of var
        '~ <> ^ get/any var
    ]
]

undefined?: func [
    {Determine if a variable is "unattached" or unset}
    return: [logic?]
    var [word! path! tuple!]
][
    if tuple? var [
        return did trap [get var]  ; can't use BINDING OF on TUPLE! atm
    ]
    return did any [
        '~attached~ = binding of var
        '~ = ^ get/any var
    ]
]

voided?: func [
    {Determine if a variable looks up to a void}
    return: [logic?]
    var [word! path! tuple!]
][
    return void? get/any var
]


curtail: reframer func [
    {Voids an expression if it raises any NEED-NON-NULL failures}
    return: [<void> <opt> any-value!]
    frame [frame!]
][
    return do frame except e -> [
        if e.id == 'need-non-null [return void]
        fail e
    ]
]


; >- is the SHOVE operator.  It uses the item immediately to its left for
; the first argument to whatever operation is on its right hand side.
; Parameter conventions of that first argument apply when processing the
; value, e.g. quoted arguments will act quoted.
;
; By default, the evaluation rules proceed according to the enfix mode of
; the operation being shoved into:
;
;    >> 10 >- lib.= 5 + 5  ; as if you wrote `10 = 5 + 5`
;    ** Script Error: + does not allow logic! for its value1 argument
;
;    >> 10 >- equal? 5 + 5  ; as if you wrote `equal? 10 5 + 5`
;    == ~true~  ; isotope
;
; You can force processing to be enfix using `->-` (an infix-looking "icon"):
;
;    >> 1 ->- lib.add 2 * 3  ; as if you wrote `1 + 2 * 3`
;    == 9
;
; Or force prefix processing using `>--` (multi-arg prefix "icon"):
;
;    >> 10 >-- lib.+ 2 * 3  ; as if you wrote `add 1 2 * 3`
;    == 7
;
>-: enfixed :shove
>--: enfixed specialize :>- [prefix: true]
->-: enfixed specialize :>- [prefix: false]


; The -- and ++ operators were deemed too "C-like", so ME was created to allow
; `some-var: me + 1` or `some-var: me / 2` in a generic way.  Once they shared
; code with SHOVE, but are currently done using macro due to unfinished binding
; semantics in SHOVE pertaining to fetched values.

me: enfixed redescribe [
    {Update variable using it as the left hand argument to an enfix operator}
](
    macro ['left [set-word! set-tuple!] 'right [word! tuple! path!]] [
        :[left, plain left, right]
    ]
)

my: enfixed redescribe [
    {Update variable using it as the first argument to a prefix operator}
](
    macro ['left [set-word! set-tuple!] 'right [word! tuple! path!]] [
        :[left, right, plain left]
    ]
)

so: enfixed func [
    {Postfix assertion which won't keep running if left expression is false}

    return: [<opt> <void> any-value!]
    condition "Condition to test, must resolve to a LOGIC! (use DID, NOT)"
        [logic!]
    feed "Needs value to return as result e.g. (x: even? 4 so 10 + 20)"
        [<end> <opt> <void> any-value! <variadic>]
][
    if not condition [
        fail 'condition make error! [
            type: 'Script
            id: 'assertion-failure
            arg1: compose [~false~ so]
        ]
    ]
    if tail? feed [return none]
    return take feed
]
tweak :so 'postpone on


was: enfixed redescribe [
    "Assert that the left hand side--when fully evaluated--IS the right"
](
    lambda [left [<opt> any-value!] right [<opt> any-value!]] [
        if :left != :right [
            fail 'return make error! [
                type: 'Script
                id: 'assertion-failure
                arg1: compose [(:left) is (:right)]
            ]
        ]
        :left  ; choose left in case binding or case matters somehow
    ]
)
tweak :was 'postpone on


zdeflate: redescribe [
    {Deflates data with zlib envelope: https://en.wikipedia.org/wiki/ZLIB}
](
    specialize :deflate [envelope: 'zlib]
)

zinflate: redescribe [
    {Inflates data with zlib envelope: https://en.wikipedia.org/wiki/ZLIB}
](
    specialize :inflate [envelope: 'zlib]
)

gzip: redescribe [
    {Deflates data with gzip envelope: https://en.wikipedia.org/wiki/Gzip}
](
    specialize :deflate [envelope: 'gzip]
)

gunzip: redescribe [
    {Inflates data with gzip envelope: https://en.wikipedia.org/wiki/Gzip}
](
    specialize :inflate [envelope: 'gzip]  ; What about GZIP-BADSIZE?
)

ensure: redescribe [
    {Pass through value if it matches test, otherwise trigger a FAIL}
](
    ; MATCH returns a pack (isotopic block) vs. NULL if the input is NULL
    ; and matches NULL.  This is not reactive with ELSE
    ;
    enclose :match lambda [f <local> value] [  ; LET was having trouble
        value: :f.value  ; DO makes frame arguments unavailable
        do f else [
            ; !!! Can't use FAIL/WHERE until we can implicate the callsite.
            ;
            ; https://github.com/metaeducation/ren-c/issues/587
            ;
            fail [
                "ENSURE failed with argument of type"
                    kind of :value else ["VOID"]
            ]
        ]
    ]
)

non: redescribe [
    {Pass through value if it *doesn't* match test, else null (e.g. MATCH/NOT)}
](
    enclose :match lambda [f] [
        let value: :f.value  ; DO makes frame arguments unavailable
        light (do f then [null] else [:value])
    ]
)

prohibit: redescribe [
    {Pass through value if it *doesn't* match test, else fail (e.g. ENSURE/NOT)}
](
    enclose :match lambda [f] [
        let value: :f.value  ; DO makes frame arguments unavailable
        do f then [
            ; !!! Can't use FAIL/WHERE until we can implicate the callsite.
            ;
            ; https://github.com/metaeducation/ren-c/issues/587
            ;
            fail [
                "NON failed with argument of type"
                    kind of :value else ["VOID"]
            ]
        ]
        :value
    ]
)


oneshot: specialize :n-shot [n: 1]
upshot: specialize :n-shot [n: -1]

;
; !!! The /REVERSE and /LAST refinements of FIND and SELECT caused a lot of
; bugs.  This recasts those refinements in userspace, in the hopes to reduce
; the combinatorics in the C code.  If needed, they could be made for SELECT.
;

find-reverse: redescribe [
    {Variant of FIND that uses a /SKIP of -1}
](
    specialize :find [skip: -1]
)

find-last: redescribe [
    {Variant of FIND that uses a /SKIP of -1 and seeks the TAIL of a series}
](
    adapt :find-reverse [
        if not any-series? series [
            fail 'series "Can only use FIND-LAST on ANY-SERIES!"
        ]

        series: tail of series  ; can't use plain TAIL due to /TAIL refinement
    ]
)

attempt: func [
    {Evaluate a block and returns result or NULL if an expression fails}

    return: "Returns NULL on failure (-or- if last evaluative result is NULL)"
        [<opt> <void> any-value!]
    code [block!]
    <local> temp
][
    if error? temp: entrap code [return null]
    return unmeta temp
]

trap: func [
    {If evaluation raises an error, return it, otherwise NULL}

    return: [<opt> any-value!]
    code [block!]
][
    return match error! entrap code
]

trap+: func [
    {Experimental variation of TRAP using THENable mechanics}

    return: [<opt> any-value!]
    code [block!]
    <local> result
][
    ; If you return a pure NULL with the desire of triggering ELSE, that does
    ; not allow you to return more values.  This uses a lazy object that will
    ; run a THEN branch on error, but then an ELSE branch with the returned
    ; value on non-error...or reify to a pack with NULL for the error and
    ; the result.
    ;
    if error? result: entrap code [
        return pack [result null]
    ]

    return isotopic make object! [
        else: branch -> [(heavy unmeta :result) then (:branch)]
        decay: [pack [null unmeta result]]
    ]
]

reduce*: redescribe [
    "REDUCE a block but vaporize NULL Expressions"
](
    specialize :reduce [predicate: unrun :maybe]
)

for-next: redescribe [
    "Evaluates a block for each position until the end, using NEXT to skip"
](
    specialize :for-skip [skip: 1]
)

for-back: redescribe [
    "Evaluates a block for each position until the start, using BACK to skip"
](
    specialize :for-skip [skip: -1]
)

iterate-skip: redescribe [
    "Variant of FOR-SKIP that directly modifies a series variable in a word"
](
    specialize enclose :for-skip func [f] [
        if blank? let word: f.word [return null]
        f.word: quote to word! word  ; do not create new virtual binding
        let saved: f.series: get word

        ; !!! https://github.com/rebol/rebol-issues/issues/2331
        comment [
            let result
            trap [result: do f] then e -> [
                set word saved
                fail e
            ]
            set word saved
            :result
        ]

        return (do f, elide set word saved)
    ][
        series: <overwritten>
    ]
)

iterate: iterate-next: redescribe [
    "Variant of FOR-NEXT that directly modifies a series variable in a word"
](
    specialize :iterate-skip [skip: 1]
)

iterate-back: redescribe [
    "Variant of FOR-BACK that directly modifies a series variable in a word"
](
    specialize :iterate-skip [skip: -1]
)


count-up: func [
    "Loop the body, setting a word from 1 up to the end value given"
    return: [<opt> <void> any-value!]
    'var [word!]
    limit [<maybe> integer! issue!]
    body [block!]
    <local> start end result'
][
    ; REPEAT in UPARSE wanted to try out some cutting-edge ideas about
    ; "opting in" to counting loops, e.g. `count-up i _` opts out and doesn't
    ; loop at all.  But what if `count-up i #` meant loop forever?  This
    ; clunky layer on top of cfor is a good test of loop abstraction, and
    ; is good enough to let UPARSE do its experiment without changing any
    ; native code.

    start: 1
    end: if issue? limit [
        if limit <> # [fail]
        100  ; not forever...don't use max int to help test "pseudoforever"
    ] else [
        limit
    ]
    return cycle [
        result': ^ cfor :var start end 1 body except e -> [
            return raise e
        ]
        if result' = null' [return null]  ; a BREAK was encountered
        if result' = void' [
            assert [start = end]  ; should only happen if body never runs
            return void'
        ]
        if limit <> # [  ; Note: /WITH not ^META, decays PACK! etc
            stop/with heavy unmeta result'  ; the limit was actually reached
        ]
        ; otherwise keep going...
        end: end + 100
        start: start + 100
    ]
]

count-down: redescribe [
    "Loop the body, setting a word from the end value given down to 1"
](
    specialize adapt :cfor [
        start: end
        end: 1
    ][
        start: <overwritten-with-end>
        bump: -1
    ]
)


lock-of: redescribe [
    "If value is already locked, return it...otherwise CLONE it and LOCK it."
](
    chain [specialize :copy [deep: #], :freeze]
)

eval-all: func [
    {Evaluate any number of expressions and discard them}

    return: [nihil?]
    expressions [<opt> any-value! <variadic>]
        {Any number of expressions on the right.}
][
    do expressions
    return nihil
]


; These constructs used to be enfix to complete their left hand side.  Yet
; that form of completion was only one expression's worth, when they wanted
; to allow longer runs of evaluation.  "Invisible functions" (those which
; `return: <void>`) permit a more flexible version of the mechanic.

|<\||: runs tweak copy unrun :eval-all 'postpone on
|\|>|: runs tweak enfixed :shove 'postpone on


meth: enfix func [
    {FUNC variant that creates an ACTION! implicitly bound in a context}

    return: [activation!]
    :member [set-word! set-path!]
    spec [block!]
    body [block!]
    /gather "Temporary compatibility tweak for METHOD (until synonymous)"
][
    let context: binding of member else [
        fail [member "must be bound to an ANY-CONTEXT! to use METHOD"]
    ]
    return set member runs bind (  ; !!! BIND doesn't take ACTION! as isotope
        unrun apply :func [
            compose [(spread spec) <in> (context)]
            body
            /gather gather
        ]
    ) context
]


cause-error: func [
    "Causes an immediate error throw with the provided information."
    err-type [word!]
    err-id [word!]
    args
][
    args: blockify args  ; make sure it's a block

    fail make error! [
        type: err-type
        id: err-id
        arg1: try first args
        arg2: try second args
        arg3: try third args
    ]
]


; Note that in addition to this definition of FAIL, there is an early-boot
; definition which runs if a FAIL happens before this point, which panics and
; gives more debug information.
;
; !!! Should there be a special bit or dispatcher used on the FAIL to ensure
; it does not continue running?  `return: <divergent>`?
;
; Though HIJACK would have to be aware of it and preserve the rule.
;
raise: func [
    {Interrupts execution by reporting an error (a TRAP can intercept it).}

    return: []  ; !!! notation for divergent function?
    'blame "Point to variable or parameter to blame"
        [<skip> quoted!]
    reason "ERROR! value, ID, URL, message text, or failure spec"
        [<end> error! path! url! text! block! isoword?]
    /where "Frame or parameter at which to indicate the error originated"
        [frame! any-word!]
][
    if (isoword? reason) and (not null? reason) [  ; <end> acts as null nonmeta
        ;
        ; !!! It's not clear that users will be able to create arbitrary
        ; isotopic words like ~unreachable~ to occupy the same space as
        ; ~null~, ~true~, ~false~, etc.  But for a time they were allowed
        ; to say things like (fail ~unreachable~)...permit for now.
        ;
        reason: noisotope reason
    ]
    all [error? reason, not blame, not where] then [
        return raise* reason  ; fast shortcut
    ]
    if blame [
        blame: (match [word! tuple!] unquote blame) else [
            fail "Quoted blame for error must be WORD! or TUPLE!)"
        ]
    ]

    ; Ultimately we might like FAIL to use some clever error-creating dialect
    ; when passed a block, maybe something like:
    ;
    ;     fail [<invalid-key> {The key} key-name: key {is invalid}]
    ;
    ; That could provide an error ID, the format message, and the values to
    ; plug into the slots to make the message...which could be extracted from
    ; the error if captured (e.g. error.id and `error.key-name`.  Another
    ; option would be something like:
    ;
    ;     fail/with [{The key} :key-name {is invalid}] [key-name: key]

    ; !!! PATH! doesn't do BINDING OF, and in the general case it couldn't
    ; tell you where it resolved to without evaluating, just do WORD! for now.
    ;
    let frame: match frame! maybe binding of maybe match word! blame

    let error: switch/type :reason [
        error! [reason]
        text! [make error! reason]
        word! [
            make error! [
                type: 'User
                id: reason
                message: to text! reason
            ]
        ]
        path! [
            if word? last reason [
                make error! [
                    type: 'User
                    id: last reason
                    message: to text! reason
                ]
            ] else [
                make error! to text! reason
            ]
        ]
        url! [make error! to text! reason]  ; should use URL! as ID
        block! [
            make error! (spaced reason else '[
                type: 'Script
                id: 'unknown-error
            ])
        ]
    ] else [
        null? reason so make error! compose [
            type: 'Script
            (spread case [
                frame and (blame) '[
                    id: 'invalid-arg
                    arg1: label of frame
                    arg2: as word! uppercase make text! blame
                    arg3: get blame
                ]
                frame and (not blame) '[
                    id: 'no-arg
                    arg1: label of frame
                    arg2: as word! uppercase make text! blame
                ]
                blame and (get blame) '[
                    id: 'bad-value
                    arg1: get blame
                ]
            ] else '[
                id: 'unknown-error
            ])
        ]
    ]

    if not pick error 'where [
        ;
        ; If no specific location specified, and error doesn't already have a
        ; location, make it appear to originate from the frame calling FAIL.
        ;
        where: default [any [frame, binding of 'return]]

        set-location-of-error error where  ; !!! why is this native?
    ]

    ; Initially this would call DO in order to force an exception to the nearest
    ; trap up the stack (if any).  However, Ren-C rethought errors as being
    ; "definitional", which means you would say RETURN RAISE and it would be
    ; a special kind of "error isotope" that was a unique return result.  Then
    ; you typically can only trap/catch errors that come from a function you
    ; directly called.
    ;
    return raise* ensure error! error
]

; Immediately fail on a raised error--do not allow ^META interception/etc.
;
; Note: we use CHAIN into NULL? as an arbitrary intrinsic which won't take
; a raised error, because the CHAIN doesn't add a stack level that obscures
; generation of the NEAR and WHERE fields.  If we tried to ENCLOSE and DO
; the error it would add more overhead and confuse those matters.
;
fail: chain [:raise, :null?]
