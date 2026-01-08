; %identity.test.r
; 
; The identity function comes in a variation called GHOSTLY which is
; vanishable, and can be used in place of the ^ operator.
;

(1 = identity 1)
('~('10 '20)~ = lift identity pack [10 20])

('~,~ = lift identity ()) 
('~,~ = lift ghostly ())

('~()~  = lift (1000 + 20 identity ()))  ; not vanishable function
((the '1020) = lift (1000 + 20 ghostly ()))
