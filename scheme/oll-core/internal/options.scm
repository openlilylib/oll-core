(define-module (oll-core internal options))

(use-modules
 (lily)
 (srfi srfi-1)
 (oll-core internal predicates))

; Convenience functions to add some type checking
; to the bundling of package option templates.
; To be used by package creators only
(define (make-mandatory-props props)
  (if (oll-mand-props? props)
      props
      (begin
       (oll:warn "Wrong argument type: oll-mand-props? expected")
       '())))

(define (make-accepted-props props)
  (if (oll-accepted-props? props)
      props
      (begin
       (oll:warn "Wrong argument type: oll-accepted-props? expected")
       '())))

; Convenience function, retrieves a property alist from a context-mod object.
; Properties can optionally be mandatory and type-checked
(define (validate-props rules props)
  "Check a list of properties and return a possibly updated list.
    - Handle unknown options (remove or not depending on 'strict' or 'flexible' ruleset)
    - type check
    - Handle missing properties. If a default is available use that.
    It is *assumed* without checking that rules satisfies the prop-rules? predicate,
    which can be justified because the function should not be called from documents."
  (let*
   ((strict (eq? (car rules) 'strict))
    (rules (cdr rules))
    (rules
     (map (lambda (rule)
            (let*
             ((rule (if (symbol? rule) (list rule) rule))
              (optional (eq? (first rule) '?))
              (rule (if optional (cdr rule) rule))
              (k (first rule))
              (pred
               (if (= (length rule) 1)
                   scheme?
                   (second rule)))
              (default
               (if (= 3 (length rule))
                   (third rule)
                   '())))
             (list k pred default optional)))
       rules))
    (missing
     (delete '()
       (map (lambda (r)
              (let*
               ((k (car r))
                (default (third r))
                (optional (fourth r))
                (prop (assoc-get k props)))
               (cond
                (prop '())
                ((not (null? default)) (cons k default))
                (optional '())
                (else
                 (begin
                  (ly:input-warning (*location*)
                    "Missing mandatory property \"~a\"." k)
                  '())))))
         rules)))
    (props
     (delete '()
       (map (lambda (p)
              (let*
               ((k (car p)) (v (cdr p)) (rule (assoc-ref rules k)))
               (cond
                ;; unknown option
                ((not rule)
                 (if strict
                     (begin
                      (ly:input-warning (*location*)
                        "Unknown property \"~a\"." k)
                      '())
                     p))
                ;; type check successful
                (((car rule) v) p)
                (else
                 (begin
                  (ly:input-warning (*location*)
                    "Type check failed for property \"~a\".\nExpected: ~a, given: ~a"
                    k (car rule) v)
                  '())))))
         props)))
    )
   (append props missing)))


; Convert a ly:context-mod? argument to a properties alist
; Arguments:
; - rules (optional): a prop-rules? property definition list
; - mod: the context-mod
(define-public context-mod->props
  (lambda (req . rest)
    ;unpack mod and rules from the arguments
    (let ((mod
           (cond
            ((ly:context-mod? req) req)
            ((and (= 1 (length rest)) (ly:context-mod? (first rest))) (first rest))
            (else
             (begin
              (ly:error "context-mod->props didn't receive a context-mod")
              (ly:make-context-mod)))))
          (rules (if (prop-rules? req)
                     req
                     '(flexible))))
      (let
       ((props
         (map
          (lambda (prop)
            (cons (cadr prop) (caddr prop)))
          (ly:get-context-mods mod))))
       (if rules
           (validate-props rules props)
           props)))))

(define (make-opts-function-declaration proc vars preds rules optional . body)
  "Return the declaration of a function with the given arguments."
  (let* ((vars (append '(opts) vars))
         (preds
          (if optional
              (begin
               (cond
                ((every list? preds)
                 (ly:warning "defining a with-options function without mandatory arguments."))
                ((list? (first preds))
                 (ly:warning "defining a with-options function where the first argument is optional.")))
               (append '((ly:context-mod? (ly:make-context-mod))) preds))
              (append '(ly:context-mod?) preds)))
         (rules
          (if (empty-parens? rules)
              (quote '(flexible))
              rules)))
    `(,proc ,vars ,preds
       (let* ((rules ,rules)
              (props (context-mod->props rules opts))
              (property (lambda (name) (assq-ref props name)))
              )
         . ,body))))

(export make-mandatory-props)
(export make-accepted-props)
(export validate-props)
(export context-mod->props)
(export make-opts-function-declaration)
