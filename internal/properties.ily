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
     (setChildOptions root
       `((presets ())
         (use-only-presets #f)
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
    ((propset-path (append '(_propsets) propset-path))
     ;; locate the property set
     (propset
      (let ((branch (getAtree #t 'oll-options propset-path)))
        (if (pair? branch) (cdr branch) #f)))
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
