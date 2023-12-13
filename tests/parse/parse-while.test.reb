; %parse-while.test.reb
;
; This is a test for UPARSE's new proposed arity-2 WHILE, which is called UWHILE
; in order to reduce confusion until all extant WHILEs are gone from PARSE.
;
; It doesn't test for truthiness or falseyness of the rule--which is a bit
; confusing, rather whether the rule succeeded.  Hence tests for logic need to
; be done with GET-GROUP! on imperative code.

(parse [1 2 3] [(x: 0) while :(x < 3) [x: integer!]])

(1020 = parse "aaa" [while [some "a"] (1020)])

(trash? parse "" [while [some "a"] (1020)])
