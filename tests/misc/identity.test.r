; %identity.test.r
;
; The identity function is vanishable, for parity with the `^` operator.
;

(1 = identity 1)
('~('10 '20)~ = lift identity pack [10 20])

('~,~ = lift identity ())

((the '1020)  = lift (1000 + 20 identity ()))
