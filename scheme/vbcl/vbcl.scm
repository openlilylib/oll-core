;; Very Basic Config Language
;;
;; VBCL parser
;;
;; Andrew Bernard 2017
;;
;; for guile 1.8

(define-module (oll scheme vbcl))
(export parse-vbcl-config)

(use-modules (ice-9 regex))
(use-modules (ice-9 rdelim))
(use-modules (srfi srfi-1))
;;(use-modules (local control))


;; parse config file.
;; return an alist of settings.

(define parse-vbcl-config
  (lambda (file)
    (let ((h (open-input-file file)) ; state vars
	  (m #f)
	  (result '())
	  (done #f))

      ;; helper functions
      (define matcher
	(lambda (pat line)
	  (set! m (string-match pat line))
	  (if m #t #f)))

      ;; main code body
      (let lp ((line (read-a-line h)))
	(if (eof-object? (peek-char h))
	    (begin
	      ;; done
	      (close-input-port h)
	      result)
	    (begin
	      (cond
	       ;; comments
	       ((matcher "^#" line)
		#t)
	       
	       ;; long text
	       ((matcher "((.*): <)" line)
		;; put the pair in the alist. the data is a string of lines.
		(set! result (cons
			      (cons
			       (string-trim-right (match:substring m 2))
			       (parse-long-textline-entries h))
			      result)))

	       ;; lists
	       ((matcher "((.*): \\[)" line)
		;; put the pair in the alist. the data is a vector.
		(set! result (cons
			      (cons
			       (string-trim-right (match:substring m 2))
			       (list->vector (parse-list-entries h)))
			      result)))

	       ;; name value pairs
	       ((matcher  "^[ \t]*(.*): (.*)" line)
		;; put the pair in the alist
		(set! result (cons
			      (cons
			       (string-trim-right (match:substring m 1))
			       (string-trim-right (match:substring m 2)))
			      result)))
	       ) ; cond
	      
	      (lp (read-a-line h))
	      ) ; begin

	    )))))

;; inner loop processing, most easily isolated using functions

(define parse-long-textline-entries
  (lambda (h)

    ;; return string of lines until end condition found - the delimiter
    ;; for the type of object.  e.g. > or ]

    ;; needs to be a separate function to avoid altering the state in
    ;; the context from which it is run.

    (let* ((m #f) 
	   (data "")
	   (line (read-a-line h)))

      ;; helper
      (define matcher
	(lambda (pat line)
	  (set! m (string-match pat line))
	  (if m #t #f)))

      ;; main code body
      (while #t
	     (cond
	      ((matcher "^[ \t]*>" line)
	       (break))

	      ((matcher "^  (.*)$" line)
	       (set! data (string-append data "\n" (match:substring m 1))))
	      )
	     (set! line (read-a-line h))
	     ) ; while
      data
      )))

(define parse-list-entries
  (lambda (h)

    ;; return list of lines until end condition found - the delimiter
    ;; for the type of object.  e.g. > or ]

    ;; needs to be a separate function to avoid altering the state in
    ;; the context from which it is run.
    
    (let* ((m #f)
	   (result '())
	   (line (read-a-line h)))

      ;; helper
      (define matcher
	(lambda (pat line)
	  (set! m (string-match pat line))
	  (if m #t #f)))

      ;; main code body
      (while #t
	     (cond
	      ((matcher "^[ \t]*]" line)
	       (break))

	      ((matcher "^  (.*)$" line)
	       (set! result (cons (match:substring m 1) result)))
	      )
	     (set! line (read-a-line h))
	     ) ; while
      result
      )))

;; read line char at a time
;; (because read-line seems to have strange issues, or maybe not!)
(define read-a-line
  (lambda (p)
    (let lp ((c (read-char p))
	     (buf '()))
      (if (or (eof-object? c) (eqv? #\nl c))
	  (list->string (reverse buf))
	  (begin
	    (lp (read-char p) (cons c buf)))))))

