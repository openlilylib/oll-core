;; Iterators

(define-module (oll-core internal iterator))
(use-modules (ice-9 match))

;; list iterator
(define-public (list-iter lis)
  (let ((state lis))
    (lambda ()
      (match state
        ((a . b)
         (set! state b)
         a)
        (()
         'list-ended)))))
