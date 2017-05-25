;; VBCL parser test program

(use-modules (oll scheme vbcl))


(define main
  (lambda ()
    (let ((cfg (parse-vbcl-config "p.cnf")))
      (format #t "config alist:\n")
      (format #t "=================================================\n")
      (format #t "~a\n\n" cfg)
      ;; use some values
      (format #t "config:\n")
      (format #t "=================================================\n")
      (format #t "name: ~a\n" (assoc-ref cfg "name"))
      (format #t "display name: ~a\n" (assoc-ref cfg "display-name"))
      (format #t "short description: ~a\n\n" (assoc-ref cfg "short-description"))
      (format #t "description: ~a\n\n" (assoc-ref cfg "description"))
      (format #t "dependencies: ~a\n\n" (assoc-ref cfg "dependencies"))
      (format #t "oll-core: ~a\n" (assoc-ref cfg "oll-core"))
      (format #t "maintainers:\n")
      (vector-display (assoc-ref cfg "maintainers"))
      (format #t "version: ~a\n" (assoc-ref cfg "version"))
      (format #t "license: ~a\n" (assoc-ref cfg "license"))
      (format #t "repository: ~a\n" (assoc-ref cfg "repository"))
      )))

;; display vector in list form
(define vector-display
  (lambda (v)
    (let ((l (vector->list v)))
      (for-each (lambda (x) (format #t "- ~a\n" x)) l))))
