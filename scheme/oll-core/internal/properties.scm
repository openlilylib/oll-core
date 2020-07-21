(define-module (oll-core internal properties))

(use-modules
 (srfi srfi-1))

(define (get-propset-path propset-path)
   "Translate a propset path to its real storage location"
   (append '(_propsets) propset-path))


(export get-propset-path)
;(export get-propset)
;(export get-propset-props)