;; VBCL parser test program

(use-modules (oll-core internal vbcl))
(use-modules (ice-9 rdelim))

(define main
  (lambda ()
    (let* ((lines (read-lines-from-file "package.cnf"))
	   (cfg (parse-vbcl-config lines)))
      (format #t "config alist:\n")
      (format #t "=================================================\n")
      (format #t "~a\n\n" cfg)
      ;; use some values
      (format #t "config:\n")
      (format #t "=================================================\n")
      (format #t "name: ~a\n" (assq-ref cfg 'name))
      (format #t "display name: ~a\n" (assq-ref cfg 'display-name))
      (format #t "short description:\n~a\n" (assq-ref cfg 'short-description))
      (format #t "description:\n~a\n" (assq-ref cfg 'description))
      (format #t "dependencies:\n")
      (list-display (assq-ref cfg 'dependencies))
      (format #t "oll-core: ~a\n" (assq-ref cfg 'oll-core))
      (format #t "maintainers:\n")
      (list-display (assq-ref cfg 'maintainers))
      (format #t "version: ~a\n" (assq-ref cfg 'version))
      (format #t "license: ~a\n" (assq-ref cfg 'license))
      (format #t "repository: ~a\n" (assq-ref cfg 'repository))
      )))

;; display list as lines of items
(define list-display
  (lambda (l)
      (for-each (lambda (x) (format #t "- ~a\n" x)) l)))

;; read a file as a list of lines
(define read-lines-from-file
  (lambda (file)
    (if (file-exists? file)
      (let ((h (open-input-file file))
            (lines '()))
        (let lp ((line (read-line h 'concat)))
          (if (eof-object? line)
              (reverse lines)
              (begin
                (set! lines (cons line lines))
                (lp (read-line h 'concat))))))
      #f)))

(main)
