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
   "Retrieve the actual presets alist from a propset.
    Returns #f if the propset doesn't exist."
   (let ((propset (get-propset propset-path)))
     (if propset
         (assq-ref propset 'presets)
         #f)))

#(define (get-propset-preset-settings propset-path)
   "Retrieve the preset-settings alist from a propset.
    Returns #f if the propset doesn't exist."
   (let ((propset (get-propset propset-path)))
     (if propset
         (assq-ref propset 'preset-settings)
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

#(define (preset-setting? obj)
   "Preset settings may either be symbol lists or booleans."
   (or (symbol-list? obj)
       (boolean? obj)))

%
setPresetFilters =
#(define-void-function (path type value)(symbol-list? symbol? preset-setting?)
   (let*
    ((propset (get-propset path))
     (settings-path
      (if propset
          (append '(_propsets) path '(preset-settings))
          (begin
           (oll:warn "
Trying to set preset settings for non-existent property set
~a"
             (os-path-join-dots path))
           #f)))
     )
    (if (and settings-path
             (member type '(use-only-presets require-preset ignore-presets)))
        (setChildOption settings-path type value)
        (if settings-path
            (oll:warn "
Trying to set illegal preset setting '~a'
for property set ~a.
Skipping."
              type
              (os-path-join-dots path)
              )))
    ))


setGlobalPresetSettings =
#(define-void-function (type value)(symbol? preset-setting?)
   (setPresetFilters '(OLL presets) type value))

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
     (setChildOption root 'presets '())
     (setChildOption root 'preset-settings
       `((require-preset . #f)
         (use-only-presets . ())
         (ignore-presets . ())))
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



% Define a preset to be applied later.
% Pass a \with {} block with any options to be specified
% and a name.
% The property set must exist,
% each specified property must exist,
% and values must match the properties' predicates
definePreset =
#(with-required-options define-void-function
   (propset-path preset-name)(symbol-list? symbol?)
   '(flexible)
   (let ((propset (get-propset-props propset-path)))
     (if propset
         (let ((preset '())
               (preset-path (append (get-propset-path propset-path) '(presets))))
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
                        ;; add to preset
                        (set! preset
                              (assoc-set! preset prop-name value))
                        ;; typecheck failed
                        (oll:warn "
Typecheck error while defining preset
  ~a
for property set
  ~a
Property '~a': expected ~a, got ~a.
Skipping"
                          preset-name
                          (os-path-join-dots propset-path)
                          prop-name
                          (cadr entry)
                          value
                          )))
                   ;; property set exists but doesn't contain property
                   (oll:warn "
Trying to define preset for non-existent property
  ~a"
                     (append propset-path (list prop-name))))
               ))
            ;; (props variable created by with-options)
            props)
           ;; assign the new preset
           (setChildOption
            (append (get-propset-path propset-path) '(presets))
            preset-name
            preset))
         ;; no propset found
         (oll:warn "
Trying to define preset for non-existent propset
  ~a
Skipping definition."
           (os-path-join-dots propset-path)))))

#(define (symbol-list-or-boolean? obj)
   (or (symbol-list? obj)
       (boolean? obj)))

\definePropertySet OLL.presets #'()

#(define (merge-preset-settings global by-propset)
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
             ;; (NOTE: preset = #'___nothing___ would produce unexpected (?) results)
             '(___nothing___)
             result)
         )))
     (require-preset (or (assq-ref global 'require-preset)
                         (assq-ref by-propset 'require-preset)))
     (use-only (merge-func
                (assq-ref global 'use-only-presets)
                (assq-ref by-propset 'use-only-presets)))
     (ignore (lset-union eq?
               (assq-ref global 'ignore-presets)
               (assq-ref by-propset 'ignore-presets)))
     )
    `((require-preset . ,require-preset)
      (use-only-presets . ,use-only)
      (ignore-presets . ,ignore))))

#(define (use-by-preset propset-path props)
   "Property available inside a with-property-set generated function.
    Determines whether the preset (given or not) allows the application
    of the function regarding the OLL.presets use-presets/ignore-presets 
    properties.
    NOTE: This is always available as a check that returns #t or #f,
    but it is the responsibility of the function to act upon the information."
   (let*
    ((preset (assq-ref props 'preset))
     (_global (get-propset-preset-settings '(OLL presets)))
     (_by-propset (get-propset-preset-settings propset-path))
     (_preset-settings (merge-preset-settings _global _by-propset))
     (use-presets-prop (assq-ref _preset-settings 'use-only-presets))
     (ignore-presets-prop (assq-ref _preset-settings 'ignore-presets))
     (require-preset (assq-ref _preset-settings 'require-preset))
     (by-use
      (and
       (if require-preset preset #t)
       (or
        (null? use-presets-prop)
        (not preset)
        (member preset use-presets-prop))))
     (by-ignore
      (not (member preset ignore-presets-prop)))
     )
    (and by-use by-ignore)
    ))

#(define (get-configuration propset-path configuration-name)
   "Retrieve a list of properties specified in a preset.
    If a preset has a 'parent' property
    recursively fetch parents' properties too, in a way
    that children override values from parents."
   (if configuration-name
       ;; store preset's alist or an empty list
       ;; issue a warning if preset doesn't exist
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
Requesting non-existing preset
  ~a
for property set
  ~a.
Skipping"
                configuration-name
                propset-path)
              '())
             ))
       '()))



#(define (merge-props propset-path props configuration-or-opts)
   "Update function properties:
    - If a preset is requested (and exists)
      override properties with its values
    - If instance properties are given 
      they override both defaults and presets."
   (let*
    ((opts (if (ly:context-mod? configuration-or-opts)
               configuration-or-opts
               #{ \with { 
                 preset = #configuration-or-opts 
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
              (if (eq? name 'preset)
                  (cons symbol? value)
                  (assq-ref propset name)))
             )
            (if property
                (let*
                 ((pred (car property))
                  (value (string->symbol-property pred value)))
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

     (preset-name (assq-ref checked-props 'preset))
     (preset (get-configuration propset-path preset-name))

     )
    ;; override props with preset and instance properties
    (for-each
     (lambda (prop)
       (set! props (assoc-set! props (car prop) (cdr prop))))
     (append preset checked-props))
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
         ;; determine applicability by preset settings
         (use-preset (lambda () (use-by-preset propset-path props)))
         )
        . ,body))))
