(define-module (oll-core internal options))

(use-modules
 (lily)
 (srfi srfi-1)
 (oll-core internal logging)
 (oll-core internal predicates)
 (oll-core internal alist-access))

(newAtree 'oll-options)

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


(define (register-option opt-path init)
  (setAtree 'oll-options opt-path init))

; Convenience function to determine if an option is set.
; can be used to avoid warnings when trying to access unregistered options.
; Returns #t or #f
(define (option-registered? path)
     (pair? (getAtree #t 'oll-options path)))

(define set-option
(define-void-function (force-set path val) ((boolean?) symbol-list? scheme?)
   (let ((is-set (option-registered? path)))
     (if (and (not is-set) force-set)
         (begin
          (register-option path '())
          (set! is-set #t)))
     (if is-set
         (begin
          (setAtree 'oll-options path val)
          ; TODO: change to oll-log
          ;(oll:log "Option set: ~a"
          ;  (format "~a: ~a"
          ;    (os-path-join-dots path) val))
          )
         ;; reject setting unknown options and report that
         ; TODO: change to oll-warning
         ;(oll:warn "Not a valid option path: ~a" (os-path-join-dots path))
         ))))

(define (set-child-option force-set parent-path option val)
   (let ((is-set (option-registered? parent-path)))
     (if (and (not is-set) force-set)
         ;; register missing parent option
         (begin
          (register-option parent-path '())
          (set! is-set #t)))
     (if is-set
         (set-option #t (append parent-path (list option)) val)
         (oll-warn
          "Trying to add child to non-existent option: ~a"
          ;(os-path-join-dots
           parent-path
           ;)
          ))))

(define (set-child-options force-set parent-path children)
   (let ((is-set (option-registered? parent-path)))
     (if (and (not is-set) force-set)
         ;; register missing parent option
         (begin
          (registerOption parent-path '())
          (set! is-set #t)))
     (if is-set
         (for-each
          (lambda (opt)
            (set-child-option force-set parent-path (car opt) (cdr opt)))
          children)
         (oll:warn
          "Trying to add children to non-existent option: ~a"
          (os-path-join-dots parent-path)))))

(define (get-oll-option path)
   (let ((option (getAtree #t 'oll-options path)))
     (if option
         ;; getAtree has returned a pair => option is set
         (cdr option)
         ;; getAtree has returned #f
         (begin
          (oll-warn
           "Trying to access non-existent option: ~a" (os-path-join-dots path))
          #f))))

(define (get-option-with-fallback path fallback)
   (let ((option (getAtree #t 'oll-options path)))
     (if option
         (cdr option)
         fallback)))

(define (get-child-option path child)
   (symbol-list? symbol?)
   (get-oll-option (append path (list child))))

(define (get-child-option-with-fallback path child fallback)
   (get-option-with-fallback (append path (list child)) fallback))


(define (append-to-option create path val)
   (let
    ((opt
      ;; Handle non-existing option, either by creating an empty list
      ;; or by triggering the warning
      (if create
          (get-option-with-fallback path '())
          (get-option-with-fallback path #f))))
    (cond
     ((not opt)
      (oll-warn
       "Trying to append to non-existent option: ~a"
       (os-path-join-dots path)))
     ((not (list? opt))
      (oll-warn
       "Trying to append to non-list option: ~a"
       (os-path-join-dots path)))
     (else
      (set-option #f path (append opt (list val)))))))



(export make-mandatory-props)
(export make-accepted-props)
(export validate-props)
(export context-mod->props)
(export make-opts-function-declaration)
(export register-option)
(export set-option)
(export set-child-option)
(export set-child-options)
(export get-oll-option)
(export get-option-with-fallback)
(export get-child-option-with-fallback)
(export get-child-option)
(export append-to-option)
(export option-registered?) ; TODO: remove when obsolete