\version "2.19.60"
% VBCL parser test program

\include "oll-core/package.ily"

#(use-modules (oll-core scheme vbcl))

% display vector in list form
#(define vector-display
   (lambda (v)
     (let ((l (vector->list v)))
       (for-each (lambda (x) (format #t "- ~a\n" x)) l))))

#(let
  ((cfg (parse-vbcl-config (file->list "vbcl/p.cnf"))))
  (format #t "config alist:\n")
  (format #t "=================================================\n")
  (pretty-print cfg)
  (format #t "=================================================\n")
  ;; use some values
  (format #t "Some config values:\n")
  (format #t "name: ~a\n" (assoc-ref cfg "name"))
  (format #t "display name: ~a\n" (assoc-ref cfg "display-name"))
  (format #t "short description: ~a\n\n" (assoc-ref cfg "short-description"))
  (format #t "description: ~a\n\n" (assoc-ref cfg "description"))
  (format #t "dependencies:\n")
  (vector-display (assoc-ref cfg "dependencies"))
  (format #t "oll-core: ~a\n" (assoc-ref cfg "oll-core"))
  (format #t "maintainers:\n")
  (vector-display (assoc-ref cfg "maintainers"))
  (format #t "version: ~a\n" (assoc-ref cfg "version"))
  (format #t "license: ~a\n" (assoc-ref cfg "license"))
  (format #t "repository: ~a\n" (assoc-ref cfg "repository"))
  )

