(define-module (oll-core internal _properties))

(use-modules
 (srfi srfi-1)
 (lily)
 (oll-core internal options)
 (oll-core internal named-alists)
 )

(define (get-propset-path propset-path)
  "Property sets are stored within the global oll-options data structure.
   This function translaes a given propset-path to the actual path,
   but doesn't perform any validity check."
  (append '(_propsets) propset-path))

(define (get-propset propset-path)
  "Retrieve a full property set entry.
    Returns #f if no property set with the given path exists."
  (let* ((propset-path (get-propset-path propset-path))
         (branch (getAtree #t 'oll-options propset-path)))
    (if (pair? branch) (cdr branch) #f)))

(define (get-propset-props propset-path)
  "Retrieve the actual properties alist from a property set.
    Returns #f if the propset doesn't exist."
  (let ((propset (get-propset propset-path)))
    (if propset
        (assq-ref propset 'props)
        #f)))

(define (get-configuration propset-path)
  "Retrieve the currently active default configuration
    from the property set."
  (let ((propset (get-propset propset-path)))
    (if propset
        (assq-ref propset 'default-configuration)
        'default)))

(define (get-propset-configurations propset-path)
  "Retrieve the actual configurations alist from a propset.
    Returns #f if the propset doesn't exist."
  (let ((propset (get-propset propset-path)))
    (if propset
        (assq-ref propset 'configurations)
        #f)))

(define (get-propset-configuration-filters propset-path)
  "Retrieve the configuration-filters alist from a propset.
    Returns #f if the propset doesn't exist."
  (let ((propset (get-propset propset-path)))
    (if propset
        (assq-ref propset 'configuration-filters)
        #f)))

(define (property-definition? obj)
  "A property definition is a three-element list with
    - a key (symbol?)
    - a predicate
    - a default value (arbitrary Scheme element matching the predicate)
    - an optional fourth (string) short description"
  (and
   (list? obj)
   (> 5 (length obj) 2)
   (symbol? (first obj))
   (procedure? (second obj))
   ((second obj) (third obj))
   (if (= (length obj) 4)
       (string? (fourth obj)))
   ))

(define (property-definition-list? obj)
  "List of property definitions, used in the propset definition."
  (and
   (list? obj)
   (every property-definition? obj)))

(define (property-configuration-filter? obj)
  "Configuration filters may either be symbol lists or booleans."
  (or (symbol-list? obj)
      (boolean? obj)))

(define (get-property-entry propset-path name)
  "Retrieve items for a property entry addressed by a property path
    and the property name.
    Returns a list with references to
    - the overall property set
    - the actual properties alist
    - the property entry (a pair with predicate and value)
    If any of these doesn't exist it and the following entry/ies
    will be #f."
  (let*
   (;; locate the property set
     (propset (get-propset propset-path))
     ;; locate the actual property list
     (props
      (if propset
          (assq-ref propset 'props)
          #f))
     ;; locate the property entry
     (property-value
      (if props
          (assq-ref props name)
          #f)))
   (list propset props property-value)))

(define (string->symbol-property pred val)
  "Convert a string property to a symbol if the predicate is symbol?.
    This is used to allow 'simple' entry of symbol? properties, since the
    LilyPond parser is unable to do that interpretation in a \\with block.
    NOTE: This really works with symbol? only, not with predicates that
    *include* a symbol? propery (such as symbol-list-or-symbol?)"
  (if (and (string? val) (eq? pred symbol?))
      (string->symbol val)
      val))

(define (get-default-configuration propset-path)
  "Retrieve the current name of the default configuration
    for the given property set.
    This will be used if no specific configuration is requested."
  (let ((propset (get-propset propset-path)))
    (if propset
        (assq-ref propset 'default-configuration)
        'default)))

(define (merge-configuration-filters global by-propset)
  "Determine the (potentially empty) subset of configuration
   filters by global and by-propset filters."
  (let*
   ((merge-func
     ;; merge global and by-propset *lists*
     ;; result is always one out of
     ;; - an empty list => nothing is filtered out
     ;; - a non-empty list => elements are kept or filtered, depending on the caller
     ;; - a list containing ___nothing___ => all elements are filtered out.
     ;; If both lists contain elements the result is the intersection,
     ;; and if that is empty ___nothing___ is injected as an indicator
     ;; If either list is empty the other list is taken as the result.
     (lambda (g p)
       (let
        ((result
          (cond
           ((null? g) p)
           ((null? p) g)
           (else (lset-intersection eq? g p)))))
        (if (and (null? result) (not (null? g)) (not (null? p)))
            ;; The empty list is the result of intersecting two non-empty lists
            ;; merging of two non-empty lists has resulte
            ;; in an empty list. This does not mean "no filter"
            ;; but rather "filter *everything*"
            ;; (NOTE: configuration = #'___nothing___ would produce unexpected (?) results)
            '(___nothing___)
            result))))
    ;; Only consider function calls that have "a" configuration
    ;; This also includes cases when the default configuration has been changed by
    ;; \useDefaultPropertyConfiguration
    (require-configuration
     (or (assq-ref global 'require-configuration)
         (assq-ref by-propset 'require-configuration)))
    ;; If this list is non-empty only consider configurations in function calls
    ;; that *are* in the list.
    ;; This does *not* require a configuration to be present.
    (use-only (merge-func
               (assq-ref global 'use-only-configurations)
               (assq-ref by-propset 'use-only-configurations)))
    ;; If this list is non-empty ignore function calls with configurations from the list
    (ignore (lset-union eq?
              (assq-ref global 'ignore-configurations)
              (assq-ref by-propset 'ignore-configurations)))
    )
   `((require-configuration . ,require-configuration)
     (use-only-configurations . ,use-only)
     (ignore-configurations . ,ignore))))

(define (use-by-property-configuration? propset-path props)
  "Determine if the function should be considered, based on
   configuration filters.
   NOTE: This returns #t or #f and is available as a procedure in 
   every with-property-set function, but it is up to the implementation
   of the function to respond to that state."
  (let*
   ((configuration (assq-ref props 'configuration))
    (_global (get-propset-configuration-filters '(OLL global)))
    (_by-propset (get-propset-configuration-filters propset-path))
    (_configuration-filters (merge-configuration-filters _global _by-propset))
    (use-configurations-prop (assq-ref _configuration-filters 'use-only-configurations))
    (ignore-configurations-prop (assq-ref _configuration-filters 'ignore-configurations))
    (require-configuration (assq-ref _configuration-filters 'require-configuration))
    (by-use
     (and
      (if require-configuration (not (eq? configuration 'default)) #t)
      (or
       (null? use-configurations-prop)
       (eq? configuration 'default)
       (member configuration use-configurations-prop))))
    (by-ignore
     (not (member configuration ignore-configurations-prop)))
    )
   (and by-use by-ignore)
   ))

(define (get-configuration propset-path configuration-name)
  "Retrieve a list of properties specified in a configuration.
   If a configuration has a 'parent' property
   recursively fetch parents' properties too, in a way
   that children override values from parents."
  (if configuration-name
      ;; store configuration's alist or an empty list
      ;; issue a warning if configuration doesn't exist
      (let* ((configuration (get-propset-configurations propset-path))
             (props (assq-ref configuration configuration-name)))
        (if props
            (let* ((parent (assq-ref props 'parent))
                   (parent-props
                    (if parent
                        (get-configuration propset-path parent)
                        '()))
                   )
              (append parent-props props))
            (begin
             (oll:warn "
Requesting non-existing configuration
  ~a
for property set
  ~a.
Skipping"
               configuration-name
               propset-path)
             '())
            ))
      '()))

(define (configuration-name? obj)
  "Predicate for the choice of a configuration name.
   It is eventually handled as a symbol, but may also be #f
   to indicate 'no configuration'. A string is allowed as a convenience
   for entry in a \\with block (the automatic string->symbol-property handler
   works only for actual symbol? predicates."
  (or (symbol? obj)
      (string? obj)
      (eq? obj #f)))

(define (sanitize-configuration-name obj)
  "Ensure that a configuration name is either a symbol or #f.
   When given in a \\with block the name is typically parsed as a string,
   which must be converted, while a #f value should be kept as-is."
  (cond
   ((or (symbol? obj) (eq? obj #f)) obj)
   ((string? obj) (string->symbol obj))
   (else
    (oll:warn "
Wrong property type: expecting symbol, string or #f got ~a" obj)
    obj)))

(define (merge-props propset-path props configuration-or-opts)
  "Update function properties:
  - Base on current property set defaults
  - If a configuration is requested (and exists)
    override properties with its values
  - If instance properties are given 
    they override both defaults and configurations."
  (let*
   ((opts
     (cond
      ((ly:context-mod? configuration-or-opts)
       ;; if a \with block is given but without a configuration
       ;; the current default configuration is retrieved
       (if (member 'configuration (map cadr (ly:get-context-mods configuration-or-opts)))
           configuration-or-opts
           (begin
            (ly:add-context-mod configuration-or-opts
              `(assign configuration ,(get-default-configuration propset-path)))
            configuration-or-opts)))
      ((eq? configuration-or-opts #f)
       ;; if neither a \with block nor a configuration name is given
       ;; construct a \with block with only the current default configuration
       (let*
        ((root (get-propset propset-path))
         (current-configuration (assq-ref root 'default-configuration)))
        #{
          \with {
            configuration = #current-configuration
           }
        #}))
      (else
       ;; if only a configuration name is given
       ;; construct a \with block around it.
       #{
         \with {
           configuration = #configuration-or-opts
         }
       #})))
    ;; properties given explicity in the function call
    (given-props (context-mod->props opts))
    ;; validate and complete the properties
    (checked-props
     (let*
      ((propset (get-propset-props propset-path)))
      (filter
       (lambda (elt) (if elt #t #f))
       (map
        (lambda (prop)
          (let*
           ((name (car prop))
            (value (cdr prop))
            (property
             ;; retrieve the property as a pair of predicate and value (not checked yet)
             (if (eq? name 'configuration)
                 ;; ensure that a configuration name is either a symbol or #f
                 (cons configuration-name? (sanitize-configuration-name value))
                 (assq-ref propset name)))
            )
           (if property
               ;; type-check the property
               (let*
                ((pred (car property))
                 ;; auto-process string values if the predicate is symbol?
                 (value (string->symbol-property pred (cdr property))))
                (if (pred value)
                    ;; type-check passed => return pair with name and value
                    (cons name value)
                    (begin
                     ;; or issue a warning and return #f
                     (oll:warn "
Typecheck error for property '~a':
Expected ~a,
found ~a"
                       name pred value)
                     #f)))
               (begin
                ;; discard explicit properties not present in the property set
                (oll:warn "
Skipping property ~a = ~a
not present in property set ~a"
                  name value (os-path-join-dots propset-path))
                #f))))
        given-props))))
    ;; create another list with properties from the (default or explicit) configuration
    (configuration-name (assq-ref checked-props 'configuration))
    (configuration (get-configuration propset-path configuration-name))
    )
   ;; override props with configuration and instance properties
   (for-each
    (lambda (prop)
      (set! props (assoc-set! props (car prop) (cdr prop))))
    (append configuration checked-props))
   props))

(export
 get-propset-path
 get-propset
 get-propset-props
 get-propset-configuration
 get-propset-configurations
 get-propset-configuration-filters
 property-definition?
 property-definition-list?
 property-configuration-filter?
 get-property-entry
 string->symbol-property
 get-default-configuration
 merge-configuration-filters
 use-by-property-configuration?
 get-configuration
 configuration-name?
 sanitize-configuration-name
 merge-props
 )
