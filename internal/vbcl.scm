;; Very Basic Config Language
;;
;; VBCL parser
;;
;; Andrew Bernard 2017
;;
;; for guile 1.8

(define-module (oll-core internal vbcl))
(export
 parse-vbcl-config)

(use-modules (ice-9 regex))
(use-modules (ice-9 rdelim))
(use-modules (srfi srfi-1))
(use-modules (oll-core internal iterator))


;; parse VBCL config file.
;; return an alist of settings.

(define parse-vbcl-config
  (lambda (lines)
    (let ((m #f)
	  (result '())
	  (iter (list-iter lines)))

      ;; helper functions
      (define matcher
	(lambda (pat line)
	  (set! m (string-match pat line))
	  (if m #t #f)))

      ;; main code body
      (let outer-lp ((elem (iter)))
	(if (equal? 'list-ended elem)
	    (begin
	      ;; done
	      result)
	    (begin
	      (cond
	       ;; comments
	       ((matcher "^#" elem)
		#t)

	       ;; long text
	       ((matcher "^[[:space:]]*(.*):[[:space:]]*<" elem)
		;; put the pair in the alist. the data is a string of lines.
		(set! result
		      (cons
		       (cons
			(string->symbol (string-trim-right (match:substring m 1)))
			(parse-long-textline-entries iter))
		       result)))

	       ;; lists
	       ((matcher "^[[:space:]]*(.*):[[:space:]]*\\[" elem)
	       	;;put the pair in the alist. the data is a vector.
	       	(set! result (cons
	       		      (cons
			       (string->symbol (string-trim-right (match:substring m 1)))
	       		       (parse-list-entries iter))
	       		       result)))

	       ;; name value pairs
	       ((matcher "^[[:space:]]*(.*):[[:space:]]+(.*)" elem)
		;;put the pair in the alist.
		(set! result (cons
			      (cons
			       (string->symbol (string-trim-right (match:substring m 1)))
			       (string-trim-right (match:substring m 2)))
			      result)))
	       )
	      (outer-lp (iter))
	      ))))))

;; inner loop processing, most easily isolated using functions

(define parse-long-textline-entries
  (lambda (iter)

    ;; return string of lines until end condition found - the delimiter
    ;; for this type of object: '>'.

    ;; needs to be a separate function to avoid altering the state in
    ;; the context from which it is run.

    (let ((m #f)
	   (data ""))

      ;; helper
      (define matcher
	(lambda (pat line)
	  (set! m (string-match pat line))
	  (if m #t #f)))

      ;; main code body
      (let lp ((elem (iter)))
	(if (matcher "^[[:space:]]*>" elem)
	    data
	    (begin
	       (set! data (string-append data (string-trim-both elem #\space)))
	      (lp (iter))))))))

(define parse-list-entries
  (lambda (iter)

    ;; return list of lines until end condition found - the delimiter
    ;; for this type of object: ']'.

    ;; needs to be a separate function to avoid altering the state in
    ;; the context from which it is run.

    (let* ((m #f)
	   (result '()))

      ;; helper
      (define matcher
	(lambda (pat line)
	  (set! m (string-match pat line))
	  (if m #t #f)))

      ;; main code body
      (let lp ((elem (iter)))
	(if (matcher "^[[:space:]]*]" elem)
	    (reverse result)
	    (begin
	      (set! result (cons (string-trim-both elem) result))
	      (lp (iter))))))))
