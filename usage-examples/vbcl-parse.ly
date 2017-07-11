\version "2.19.60"
% VBCL parser test program

\include "oll-core/package.ily"

#(use-modules (oll-core internal vbcl))
#(use-modules (oll-core internal file-handling))

% display list as lines of items
#(define list-display
  (lambda (l)
      (for-each (lambda (x) (format #t "- ~a\n" x)) l)))

#(let*
  ((cfg-lines (read-lines-from-file "vbcl/package.cnf")))
  (if cfg-lines
      (let ((cfg (parse-vbcl-config cfg-lines)))
       (format #t "config alist:\n")
       (format #t "=================================================\n")
       (pretty-print cfg)
       (format #t "=================================================\n")
       ;; use some values
       (format #t "Some config values:\n")
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
     )
   (format #t "Config file not found\n")
 ))
