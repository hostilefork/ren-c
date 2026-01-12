; %except.test.r
;
; Like CATCH from some other languages, as an infix operation in the
; spirit of THEN/ELSE which reacts to failures.  (THEN and ELSE do not,
; passing any failures along, which will raise an alarm if not
; consumed by some ^META receiving site.)
;
; Note: errors are unstable antiforms and cn't be stored in variables.

(
   x: ~
   all [
       #X = fail "hello" then [x: #T] else [x: #E] except (e -> [x: e, #X])
       error? x
   ]
)
(
    e: sys.util/recover [
        panic "foo" then [print "THEN"] else [print "ELSE"]
    ]
    e.message = "foo"
)
(
    ghost? (^ghost except (e -> [<unused>]))
)

; Hot potatoes are tricky; they're not FAILURE! but EXCEPT should probably be
; willing to handle them.  Though it violates the convention of assuming the
; handler is passed an ERROR! non-antiform state.
;
; Trying out the idea of passing the dual as-is; handler must be ^META.
(
    ^veto except (^e -> [veto? ^e])
)
