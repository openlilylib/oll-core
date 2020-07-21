(define-module (oll-core internal predicates))

(use-modules
 (srfi srfi-1))

; predicate for check-props
; which accepts an a-list or a context-mod
(define (al-or-props? obj)
  (if (or (ly:context-mod? obj)
          (and (list? obj)
               (every pair? obj)))
      #t #f))

; temporary predicate, as this seems just too general ...
(define (alist? obj)
  (if (and (list? obj)
           (every pair? obj))
      #t #f))

; Predicate for a mandatory option:
; a three-element list consisting of
; - name (symbol?)
; - element predicate (procedure?)
; - default value (tested against predicate)
(define (oll-mand-prop? obj)
  (if (and (list? obj)
           (= (length obj) 3)
           (symbol? (first obj))
           (procedure? (second obj))
           ((second obj) (third obj)))
      #t #f))

; Predicate for mandatory options:
; list of oll-mand-prop? items
(define (oll-mand-props? obj)
  (if (and (list? obj)
           (every oll-mand-prop? obj))
      #t #f))

; Precidate for an accepted options:
; pair of property name and predicate
(define (oll-accepted-prop? obj)
  (if (and (pair? obj)
           (symbol? (car obj))
           (procedure? (cdr obj)))
      #t #f))

; Predicate for accepted options:
; list of oll-accepted-prop? items
(define (oll-accepted-props? obj)
  (if (and (list? obj)
           (every oll-accepted-prop? obj))
      #t #f))

(define (enforcement-symbol? obj)
  (or (eq? 'strict obj)
      (eq? 'flexible obj)))

(define (prop-rule? obj)
  "Check if obj is a property rule. A property rule can have 5 forms:
    - arg-name                    (a symbol, stating that this argument is required)
    - (arg-name)                  (a list with a symbol, same as above)
    - (arg-name ,type?)           (same as above with a type check)
    - (? arg-name)                (optional argument, for strict rulesets)
    - (? arg-name ,type?)         (same as above with a type-check)
    - (? arg-name ,type? def-v)   (same as above with a default value)
    Default values aren't checked in this predicate"
  (let ((obj (if (symbol? obj) (list obj) obj)))
    (and (list? obj)
         (not (null? obj))
         (let*
          ((opt (if (eq? '? (first obj)) #t #f))
           (obj (if opt (cdr obj) obj))
           (l (length obj)))
          (case l
            ((1) (symbol? (first obj)))
            ((2) (and (symbol? (first obj))
                      (procedure? (second obj))))
            ((3) (and opt
                      (symbol? (first obj))
                      (procedure? (second obj))))
            (else #f))))))

(define (prop-rules? obj)
  "Check if given object is a property rules structure.
    This is true when obj:
    - is a list
    - its first element is an 'enforcement-symbol?
    - subsequent elements are 'prop-rule? entries"
  (and (list? obj)
       (enforcement-symbol? (first obj))
       (every prop-rule? (cdr obj))))

(define (empty-parens? obj)
  (or (equal? (quote ()) obj)
      (equal? (quote '()) obj)
      (equal? (quote `()) obj)))



(export al-or-props?)
(export alist?)
(export oll-mand-prop?)
(export oll-mand-props?)
(export oll-accepted-prop?)
(export oll-accepted-props?)
(export enforcement-symbol?)
(export prop-rule?)
(export prop-rules?)
(export empty-parens?)