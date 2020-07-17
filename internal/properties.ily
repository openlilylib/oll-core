%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
% This file is part of openLilyLib,                                           %
%                      ===========                                            %
% the community library project for GNU LilyPond                              %
% (https://github.com/openlilylib)                                            %
%              -----------                                                    %
%                                                                             %
% Library: oll-core                                                           %
%          ========                                                           %
%                                                                             %
% openLilyLib is free software: you can redistribute it and/or modify         %
% it under the terms of the GNU General Public License as published by        %
% the Free Software Foundation, either version 3 of the License, or           %
% (at your option) any later version.                                         %
%                                                                             %
% openLilyLib is distributed in the hope that it will be useful,              %
% but WITHOUT ANY WARRANTY; without even the implied warranty of              %
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               %
% GNU General Public License for more details.                                %
%                                                                             %
% You should have received a copy of the GNU General Public License           %
% along with openLilyLib. If not, see <http://www.gnu.org/licenses/>.         %
%                                                                             %
% openLilyLib is maintained by Urs Liska, ul@openlilylib.org                  %
% and others.                                                                 %
%       Copyright Urs Liska, 2020                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Handling properties (typed options organized in property sets)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\version "2.20.0"

% Storage for all property sets
\registerOption #'(_propsets) #'()

#(define (get-propset-path propset-path)
   "Translate a propset path to its real storage location"
   (append '(_propsets) propset-path))

#(define (get-propset propset-path)
   "Retrieve a propset entry.
    Returns #f if no propset with the given path exists."
   (let* ((propset-path (get-propset-path propset-path))
          (branch (getAtree #t 'oll-options propset-path)))
     (if (pair? branch) (cdr branch) #f)))

#(define (get-propset-props propset-path)
   "Retrieve the actual properties alist from a propset.
    Returns #f if the propset doesn't exist."
   (let ((propset (get-propset propset-path)))
     (if propset
         (assq-ref propset 'props)
         #f)))

#(define (get-propset-configurations propset-path)
   "Retrieve the actual configurations alist from a propset.
    Returns #f if the propset doesn't exist."
   (let ((propset (get-propset propset-path)))
     (if propset
         (assq-ref propset 'configurations)
         #f)))

#(define (get-propset-configuration-filters propset-path)
   "Retrieve the configuration-filters alist from a propset.
    Returns #f if the propset doesn't exist."
   (let ((propset (get-propset propset-path)))
     (if propset
         (assq-ref propset 'configuration-filters)
         #f)))

#(define (property-definition? obj)
   "A property definition is a three-element list with
    - a key (symbol?)
    - a predicate
    - a default value (arbitrary Scheme element)"
   (and
    (list? obj)
    (= (length obj) 3)
    (symbol? (first obj))
    (procedure? (second obj))
    ((second obj) (third obj))
    ))

#(define (prop-list? obj)
   "List of property definitions, used in the propset definition."
   (and
    (list? obj)
    (every property-definition? obj)))

#(define (configuration-filter? obj)
   "Configuration filters may either be symbol lists or booleans."
   (or (symbol-list? obj)
       (boolean? obj)))

%
setPropertyConfFilters =
#(define-void-function (path type value)(symbol-list? symbol? configuration-filter?)
   (let*
    ((propset (get-propset path))
     (settings-path
      (if propset
          (append '(_propsets) path '(configuration-filters))
          (begin
           (oll:warn "
Trying to set property configuration filters for non-existent property set
~a"
             (os-path-join-dots path))
           #f)))
     )
    (if (and settings-path
             (member type '(use-only-configurations require-configuration ignore-configurations)))
        (setChildOption settings-path type value)
        (if settings-path
            (oll:warn "
Trying to set illegal property configuration filter '~a'
for property set ~a.
Skipping."
              type
              (os-path-join-dots path)
              )))
    ))

% Define a property set as a sequence of properties
% with names (symbol?), types (procedure), and default value.
% These are given as assignments in a \with {} block, as
%   <name> = #(list <predicate> <default>)
definePropertySet =
#(define-void-function (path prop-list)(symbol-list? prop-list?)
   (let* ((root (get-propset-path path))
          (prop-path (append root '(props))))
     ;; create structure
     (registerOption prop-path '())
     (setChildOption root 'configurations '())
     (setChildOption root 'configuration-filters
       `((require-configuration . #f)
         (use-only-configurations . ())
         (ignore-configurations . ())))
     (for-each
      (lambda (prop)
        ;; Add properties to the set.
        (setChildOption
         prop-path
         (first prop)
         (cons (second prop) (third prop))))
      (append
       `((parent ,symbol? default))
       prop-list))
     ;; create an empty 'default property configuration, as defult parent
     (definePropertyConfiguration (ly:make-context-mod) path 'default)
     ))

#(define (property-entry propset-path name)
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
      (property
       (if props
           (assq-ref props name)
           #f)))
    (list propset props property)))

% Retrieve a property value from a property set.
% If the property set doesn't exist or doesn't include the requestes property
% a warning is issued and #f returned
getProperty =
#(define-scheme-function (propset-path name)(symbol-list? symbol?)
   (let*
    ((items (property-entry propset-path name))
     (props (second items))
     (property (third items)))
    (if props
        (if property
            (cdr property)
            (begin
             (oll:warn"
Trying to access undefined property '~a'
in property set ~a.
Returns #f, please expect follow-up errors.
"
               name (os-path-join-dots propset-path))
             #f))
        (begin
         (oll:warn "
Trying to access property '~a' from
non-existent property set ~a.
Returns #f, please expect follow-up errors.
" name (os-path-join-dots propset-path))
         #f))))

#(define (string->symbol-property pred val)
   "Optionally convert a string to a symbol.
    This is used to allow 'simple' entry of symbol? properties, since the
    LilyPond parser is unable to do that interpretation in a \\with block."
   (if (and (string? val) (eq? pred symbol?))
       (string->symbol val)
       val))

% Set a property.
% If the property doesn't exist or the value doesn't match the predicate
% a warning is issued and the assignment skipped (old value kept).
setProperty =
#(define-void-function (propset-path name value)(symbol-list? symbol? scheme?)
   (let*
    ((property (third (property-entry propset-path name)))
     (prop-path (os-path-join-dots (append propset-path (list name)))))
    (if property
        (let*
         ((pred (car property))
          (value (string->symbol-property pred value)))
         (if (pred value)
             (set-cdr! property value)
             (oll:warn "
Trying to set property
  ~a
to value 
  ~a
that doesn't match the predicate
  ~a
Skipping assignment."
               prop-path
               value
               (car property))))
        (oll:warn "
Trying to set non-existent property
  ~a
Skipping assignment."
          prop-path))))

% Set multiple properties (e.g. for defining global stylesheets).
% Each assignment is validated individually, and failing assignments
% don't affect the overall outcome.
setProperties =
#(define-void-function (propset-path properties)(symbol-list? list?)
   (for-each
    (lambda (prop)
      (setProperty propset-path (car prop) (cdr prop)))
    properties))



% Define a property configuration to be applied later.
% Pass a \with {} block with any options to be specified
% and a name.
% The property set must exist,
% each specified property must exist,
% and values must match the properties' predicates
definePropertyConfiguration =
#(with-required-options define-void-function
   (propset-path configuration-name)(symbol-list? symbol?)
   '(flexible)
   (let ((propset (get-propset-props propset-path)))
     (if propset
         (let ((configuration '())
               (configuration-path (append (get-propset-path propset-path) '(configurations))))
           (for-each
            (lambda (prop)
              ;; add property if it is valid
              ;; (exists in propset and matches predicate)
              (let*
               ((prop-name (car prop))
                (value (cdr prop))
                (entry (assq prop-name propset)))
               (if entry
                   (let*
                    ((pred (cadr entry))
                     (value (if (and (string? value) (eq? symbol? pred))
                                (string->symbol value)
                                value)))
                    ;; typecheck property
                    (if (pred value)
                        ;; add to configuration
                        (set! configuration
                              (assoc-set! configuration prop-name value))
                        ;; typecheck failed
                        (oll:warn "
Typecheck error while defining configuration
  ~a
for property set
  ~a
Property '~a': expected ~a, got ~a.
Skipping"
                          configuration-name
                          (os-path-join-dots propset-path)
                          prop-name
                          (cadr entry)
                          value
                          )))
                   ;; property set exists but doesn't contain property
                   (oll:warn "
Trying to define configuration for non-existent property
  ~a"
                     (append propset-path (list prop-name))))
               ))
            ;; (props variable created by with-options)
            props)
           ;; assign the new configuration
           (setChildOption
            (append (get-propset-path propset-path) '(configurations))
            configuration-name
            configuration))
         ;; no propset found
         (oll:warn "
Trying to define configuration for non-existent propset
  ~a
Skipping definition."
           (os-path-join-dots propset-path)))))


\definePropertySet OLL.global #'()

#(define (merge-configuration-filters global by-propset)
   (let*
    ((merge-func
      (lambda (g p)
        (let
         ((result
           (cond
            ((null? g) p)
            ((null? p) g)
            (else (lset-intersection eq? g p)))))
         (if (and (null? result)
                  (or (not (null? g)) (not (null? p))))
             ;; merging of two non-empty lists has resulte
             ;; in an empty list. This does not mean "no filter"
             ;; but rather "filter *everything*"
             ;; (NOTE: configuration = #'___nothing___ would produce unexpected (?) results)
             '(___nothing___)
             result)
         )))
     (require-configuration (or (assq-ref global 'require-configuration)
                         (assq-ref by-propset 'require-configuration)))
     (use-only (merge-func
                (assq-ref global 'use-only-configurations)
                (assq-ref by-propset 'use-only-configurations)))
     (ignore (lset-union eq?
               (assq-ref global 'ignore-configurations)
               (assq-ref by-propset 'ignore-configurations)))
     )
    `((require-configuration . ,require-configuration)
      (use-only-configurations . ,use-only)
      (ignore-configurations . ,ignore))))

#(define (use-by-configuration propset-path props)
   "Property available inside a with-property-set generated function.
    Determines whether the configuration (given or not) allows the application
    of the function regarding the OLL.global use-configurations/ignore-configurations 
    properties.
    NOTE: This is always available as a check that returns #t or #f,
    but it is the responsibility of the function to act upon the information."
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

#(define (get-configuration propset-path configuration-name)
   "Retrieve a list of properties specified in a configuration.
    If a configuration has a 'parent' property
    recursively fetch parents' properties too, in a way
    that children override values from parents."
   (if configuration-name
       ;; store configuration's alist or an empty list
       ;; issue a warning if configuration doesn't exist
       (let* ((configuration (get-propset-configurations propset-path))
              (props (assq-ref configuration configuration-name))
              )
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


#(define (configuration-name? obj)
   "Predicate for the choice of a configuration name.
    It is eventually handled as a symbol, but may also be #f
    to indicate 'no configuration'. A string is allowed as a convenience
    for entry in a \\with block (the automatic string->symbol-property handler
    works only for actual symbol? predicates."
   (or (symbol? obj)
       (string? obj)
       (eq? obj #f)))

#(define (sanitize-configuration-name obj)
   "Ensure that a configuration name is either a symbol or #f.
    When given in a \\with block the name is typically parsed as a string,
    which must converted, while a #f value should be kept as-is."
   (cond
    ((or (symbol? obj) (eq? obj #f)) obj)
    ((string? obj) (string->symbol obj))
    (else
     (oll:warn "
Wrong property type: expecting string or symbol, got ~a" obj)
     obj)))

#(define (merge-props propset-path props configuration-or-opts)
   "Update function properties:
    - If a configuration is requested (and exists)
      override properties with its values
    - If instance properties are given 
      they override both defaults and configurations."
   (let*
    ((opts (if (ly:context-mod? configuration-or-opts)
               configuration-or-opts
               #{ \with {
                 configuration = #configuration-or-opts
               } #}
               ))
     (given-props (context-mod->props opts))
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
              (if (eq? name 'configuration)
                  ;; ensure that a configuration name is either a symbol or #f
                  (cons configuration-name? (sanitize-configuration-name value))
                  (assq-ref propset name)))
             )
            (if property
                (let*
                 ((pred (car property))
                  (value (string->symbol-property pred (cdr property))))
                 (if (pred value)
                     (cons name value)
                     (begin
                      (oll:warn "
Typecheck error for property '~a':
Expected ~a,
found ~a"
                        name pred value)
                      #f)))
                (begin
                 (oll:warn "
Skipping property ~a = ~a
not present in property set ~a"
                   name value (os-path-join-dots propset-path))
                 #f))))
         given-props))))

     (configuration-name (assq-ref checked-props 'configuration))
     (configuration (get-configuration propset-path configuration-name))

     )
    ;; override props with configuration and instance properties
    (for-each
     (lambda (prop)
       (set! props (assoc-set! props (car prop) (cdr prop))))
     (append configuration checked-props))
    props))

#(define (symbol-or-context-mod? obj)
   (or (symbol? obj)
       (ly:context-mod? obj)))

#(define-macro (with-property-set proc vars preds propset-path . body)
   "Create a music/scheme/void-function attached to a propset.
    The first mandatory item after the predicate list is a quasi-quoted
    symbol-list? with the propset-path to the property set governing the function.
    The resulting function will implicitly have an optional context-mod?
    as first argument. This results in the limitation that at least one mandatory
    argument must be defined. If that is not necessary one can alternatively
    accept a scheme? or ly:music? argument and return that unmodified.`
    "
   (let*
    ((vars (append `(configuration-or-opts) vars))
     (preds
      (begin
       (cond
        ((every list? preds)
         (oll:error "with-property-set functions must have at least one mandatory argument.
(You may use a dummy scheme? or ly:music? argument and simply return that unchanged.)"))
        ((list? (first preds))
         (oll:error "with-property-set functions must not have an optional argument in first position.")))
       (append '((symbol-or-context-mod? 'default)) preds)
       ))
     (propset `(get-propset-props ,propset-path))
     (props
      `(if ,propset
           ;; store a flat alist with the current property set values
           (map (lambda (prop)
                  (cons (car prop) (cddr prop)))
             ,propset)
           ;; function specified propset that isn't available
           (oll:error "
Specifying a with-property-set function with
non-existent property set '~a'" (os-path-join-dots propset-path))))

     )
    ;; let macro return function
    `(,proc ,vars ,preds
       (let*
        ((propset-path ,propset-path)
         (props (merge-props propset-path ,props configuration-or-opts))
         ;; retrieve value of a given property
         (property (lambda (name) (assq-ref props name)))
         ;; determine applicability by configuration filters
         (use-configuration (lambda () (use-by-configuration propset-path props)))
         )
        . ,body))))
