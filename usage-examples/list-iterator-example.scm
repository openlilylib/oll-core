;; List iterator example
;;
;; Andrew Bernard 2017

(use-modules (oll-core internal iterator))

(define main
  (lambda ()
    (let* ((l (iota 10)))
      (iterate-entire-list l)
      (newline)
      (iterate-list-nested l))))

(define iterate-entire-list
  (lambda (lis)
    (let* ((i (list-iter lis)))
      (format #t "iterate whole list\n")
      (let lp ((elem (i)))
	(if (not (equal? 'list-ended elem))
	    (begin
	      (format #t "~a\n" elem)
	      (lp (i))))))))

(define iterate-list-nested
  (lambda (lis)
    (let* ((i (list-iter lis)))
      (format #t "iterate list nested\n")
      (let outer-lp ((elem (i)))
	(if (not (equal? 'list-ended elem)) ;; outer loop end condition test
	    (begin
	      (if (equal? 4 elem) ;; inner loop start condition test
		  (begin
		    (let inner-lp ((elt elem))
		      (if (not (equal? 7 elt)) ;; inner loop end condition test
			  (begin
			    (format #t "-> ~a\n" elt)
			    (inner-lp (i)))
			  (begin
			    (outer-lp elt)))))
		  (begin
		    (format #t "~a\n" elem)
		    (outer-lp (i))))))))))
