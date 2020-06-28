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
   "Translate a propset path to its real storage"
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

#(define (get-propset-presets propset-path)
   "Retrieve the actual presets alist from a propset.
    Returns #f if the propset doesn't exist."
   (let ((propset (get-propset propset-path)))
     (if propset
         (assq-ref propset 'presets)
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

% Define a property set as a sequence of properties
% with names (symbol?), types (procedure), and default value.
% These are given as assignments in a \with {} block, as
%   <name> = #(list <predicate> <default>)
definePropset =
#(with-required-options define-void-function (path)(symbol-list?)
   ;; The with-options mechanism does *not* impose any restrictions here
   '(flexible)
   (let* ((root (append '(_propsets) path))
          (prop-path (append root '(props))))
     ;; create structure
     (registerOption prop-path '())
     (setChildOption root 'presets '())
     (setChildOptions root
       `((use-only-presets #f)
         (ignore-presets ())))
     (for-each
      (lambda (prop)
        ;; Add properties to the set.
        ;; The predicate both checks the structure of the argument
        ;; *and* type-checks of the default value.
        (if (property-definition? prop)
            (setChildOption
             prop-path
             (first prop)
             (cons (second prop) (third prop)))
            (oll:error
             (format "Error in \\definePropset. Property definition
  <name> = #(list <predicate> <default>)
expected, got\n\n~a" (cdr prop)))))
      props)
     (setChildOption root 'prop-names (map car props))
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

% Retrieve a property from a property set.
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

% Set a property.
% If the property doesn't exist or the value doesn't match the predicate
% a warning is issued and the assignment skipped (old value kept).
setProperty =
#(define-void-function (propset-path name value)(symbol-list? symbol? scheme?)
   (let*
    ((property (third (property-entry propset-path name)))
     (prop-path (os-path-join-dots (append propset-path (list name)))))
    (if property
        (if ((car property) value)
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
              (car property)))
        (oll:warn "
Trying to set non-existent property
  ~a
Skipping assignment."
          prop-path))))


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
                   ;; typecheck property
                   (if ((cadr entry) value)
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
                         ))
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

#(define (merge-props propset-path props opts)
   "Update function properties:
    - If a preset is requested (and exists)
      override properties with its values
    - If instance properties are given 
      they override both defaults and presets."
   (let*
    ((given-props (context-mod->props opts))
     (preset-name (assq-ref given-props 'preset))
     (preset
      (if preset-name
          ;; store preset's alist or an empty list
          ;; issue a warning if preset doesn't exist
          (let* ((presets (get-propset-presets propset-path)))
            (or (assq-ref presets
                  (string->symbol preset-name))
                (begin
                 (oll:warn "
Requesting non-existing preset
  ~a
for property set
  ~a.
Skipping"
                   preset-name
                   propset-path)
                 '())))
          '()))
     )
    ;; override props with preset and instance properties
    (for-each
     (lambda (prop)
       (set! props (assoc-set! props (car prop) (cdr prop))))
     (append preset given-props))
    props))

#(define-macro (with-propset proc vars preds propset-path . body)
   "Create a music/scheme/void-function attached to a propset.
    The first mandatory item after the predicate list is a quasi-quoted
    symbol-list? with the propset-path to the property set governing the function.
    The resulting function will implicitly have an optional context-mod?
    as first argument. This results in the limitation that at least one mandatory
    argument must be defined. If that is not necessary one can alternatively
    accept a scheme? or ly:music? argument and return that unmodified.`
    "
   (let*
    ((vars (append `(opts) vars))
     (preds
      (begin
       (cond
        ((every list? preds)
         (oll:error "with-propset functions must have at least one mandatory argument.
(You may use a dummy scheme? or ly:music? argument and simply return that unchanged.)"))
        ((list? (first preds))
         (oll:error "with-propset functions must not have an optional argument in first position.")))
       (append '((ly:context-mod? (ly:make-context-mod)) ) preds)
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
Specifying a with-propset function with
non-existent property set '~a'" (os-path-join-dots propset-path))))

     )
    ;; let macro return function
    `(,proc ,vars ,preds
       (let*
        ((propset-path ,propset-path)
         (props (merge-props propset-path ,props opts))
         ;; retrieve value of a given property
         (property (lambda (prop) (assq-ref ,props prop)))
         )
        . ,body))))
