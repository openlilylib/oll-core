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

#(use-modules
  (oll-core internal predicates)
  (oll-core internal _properties)
  )

%
setPropertyConfFilters =
#(define-void-function (path type value)(symbol-list? symbol? property-configuration-filter?)
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
#(define-void-function (path prop-list)(symbol-list? property-definition-list?)
   (let* ((root (get-propset-path path))
          (prop-path (append root '(props))))
     ;; create structure
     (registerOption prop-path '())
     (setChildOption root 'default-configuration 'default)
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
     (definePropertyConfiguration (ly:make-context-mod) (append path '(default)))
     ))

% Retrieve a property value from a property set.
% If the property set doesn't exist or doesn't include the requestes property
% a warning is issued and #f returned
getProperty =
#(define-scheme-function (propset-path name)(symbol-list? symbol?)
   (let*
    ((items (get-property-entry propset-path name))
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
    ((property (third (get-property-entry propset-path name)))
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
   (configuration-path)(symbol-list?)
   '(flexible)
   (let*
    ((propset-path (list-head configuration-path (- (length configuration-path) 1)))
     (configuration-name (last configuration-path))
     (propset (get-propset-props propset-path)))
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

% Set the property configuration that is used by default if none is given explicitly.
% This is initialized with 'default but can be changed to anything.
% Validity is checked only upon use.
usePropertyConfiguration =
#(define-void-function (propset-path configuration-name)(symbol-list? symbol?)
   (let ((propset (get-propset propset-path)))
     (if propset
         (let ((root (get-propset-path propset-path)))
           (setChildOption root 'default-configuration configuration-name))
         (oll:warn "
Trying to set '~a' as default configuration for non-existing
property set ~a.
Skipping." configuration-name (os-path-join-dots propset-path)))))

\definePropertySet OLL.global #'()

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
       ;; if no configuration is requested the current default will be used.
       (append `((symbol-or-context-mod? #f)) preds)
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
         (use-by-configuration? (lambda () (use-by-property-configuration? propset-path props)))
         )
        . ,body))))
