;; Iterators
;;
;; Andrew Bernard 2017

(define-module (oll-core scheme iterator))
(export list-iter)

;; list iterator
(define list-iter
  (lambda (lis)
    (define iter
      (lambda ()
        (call-with-current-continuation control-state)))
    (define control-state
      (lambda (return)
        (for-each
	 (lambda (element)
	   (set! return (call-with-current-continuation
			 (lambda (resume-here)
			   (set! control-state resume-here)
			   (return element)))))
	 lis)
        (return 'list-ended)))
    iter))

