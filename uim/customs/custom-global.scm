(define custom-activate-default-im-name? #t)
(define custom-preserved-default-im-name 'byeoru)
(define default-im-name 'byeoru)
(define enabled-im-list '(byeoru anthy-utf8))
(define enable-im-switch? #t)
(define switch-im-key '("<IgnoreShift><Control>)"))
(define switch-im-key? (make-key-predicate '("<IgnoreShift><Control>)")))
(define switch-im-skip-direct-im? #t)
(define enable-im-toggle? #t)
(define toggle-im-key '("<IgnoreShift><Control>,"))
(define toggle-im-key? (make-key-predicate '("<IgnoreShift><Control>,")))
(define toggle-im-alt-im 'byeoru)
(define uim-color 'uim-color-uim)
(define candidate-window-style 'vertical)
(define candidate-window-position 'caret)
(define enable-lazy-loading? #t)
(define bridge-show-input-state? #t)
(define bridge-show-with? 'mode)
(define bridge-show-input-state-time-length 3)
