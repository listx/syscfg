(define byeoru-on-key '("<Control> " "<IgnoreShift><Control>("))
(define byeoru-on-key? (make-key-predicate '("<Control> " "<IgnoreShift><Control>(")))
(define byeoru-latin-key '("<Control> " "<IgnoreShift><Control>("))
(define byeoru-latin-key? (make-key-predicate '("<Control> " "<IgnoreShift><Control>(")))
(define byeoru-conversion-key '("F9"))
(define byeoru-conversion-key? (make-key-predicate '("F9")))
(define byeoru-commit-key '(generic-commit-key))
(define byeoru-commit-key? (make-key-predicate '(generic-commit-key?)))
(define byeoru-cancel-key '(generic-cancel-key))
(define byeoru-cancel-key? (make-key-predicate '(generic-cancel-key?)))
