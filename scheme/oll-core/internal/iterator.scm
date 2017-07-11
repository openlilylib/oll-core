;; Iterators
;;
;; Andrew Bernard 2017

(define-module (oll-core internal iterator))
(export list-iter)

;; list iterator
(define list-iter
  (lambda (lis)
    (define iter
      (lambda ()
        (call/cc control-state)))
    (define control-state
      (lambda (return)
        (for-each
	 (lambda (element)
	   (set! return (call/cc
			 (lambda (resume-here)
			   (set! control-state resume-here)
			   (return element)))))
	 lis)
        (return 'list-ended)))
    iter))

