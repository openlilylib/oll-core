;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%                                                                             %
;% This file is part of openLilyLib,                                           %
;%                      ===========                                            %
;% the community library project for GNU LilyPond                              %
;% (https://github.com/openlilylib/openlilylib                                 %
;%              -----------                                                    %
;%                                                                             %
;% openLilyLib is free software: you can redistribute it and/or modify         %
;% it under the terms of the GNU General Public License as published by        %
;% the Free Software Foundation, either version 3 of the License, or           %
;% (at your option) any later version.                                         %
;%                                                                             %
;% openLilyLib is distributed in the hope that it will be useful,              %
;% but WITHOUT ANY WARRANTY; without even the implied warranty of              %
;% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               %
;% GNU General Public License for more details.                                %
;%                                                                             %
;% You should have received a copy of the GNU General Public License           %
;% along with openLilyLib. If not, see <http://www.gnu.org/licenses/>.         %
;%                                                                             %
;% openLilyLib is maintained by Urs Liska, ul@openlilylib.org                  %
;% and others.                                                                 %
;%       Copyright Urs Liska, 2015                                             %
;%                                                                             %
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comparison operators for the currently executed LilyPond version.
;; Can be used to conditionally execute code based on LilyPond version
;;
;; All operators expect a LilyPond version as a string or as a three item list.

(define-module (oll-core internal lilypond-version-predicates))

(use-modules
 (lily)
 (srfi srfi-1))

(define (deprecate func-name)
  (ly:input-warning (*location*) "

openLilyLib. DEPRECATION:
Using '~a' from oll-core is deprecated.
openLilyLib explicitly does not suppport LilyPond 2.18 anymore,
so users are expected to use the development version 2.19 or a
later stable release.
Since LilyPond 2.19.57 the version predicate 'ly:version?' is
included in LilyPond, and all uses of '~a'
should properly be replaced with that.

Reference:
http://lilypond.org/doc/v2.19/Documentation/usage/writing-code-to-support-multiple-versions
" func-name func-name))

(define (calculate-version ref-version)
  "Return an integer representation of the LilyPond version,
   can be compared with the operators."
  (let ((ver-list
         (if (list? ref-version)
             ref-version
             (let ((str-list (string-split ref-version #\.)))
               (map
                (lambda (s)
                  (string->number s))
                str-list)))))
    (+ (* 1000000 (first ver-list))
      (* 1000 (second ver-list))
      (third ver-list))))

(define-public (lilypond-greater-than? ref-version)
  "Return #t if the executed LilyPond version
   is greater than the given reference version"
  (deprecate "lilypond-greater-than?")
  (> (calculate-version (ly:version))
     (calculate-version ref-version)))

(define-public (lilypond-greater-than-or-equal? ref-version)
  "Return #t if the executed LilyPond version
   is greater than or equal to the given reference version"
  (deprecate "lilypond-greater-than-or-equal?")
  (>= (calculate-version (ly:version))
      (calculate-version ref-version)))

(define-public (lilypond-less-than? ref-version)
  "Return #t if the executed LilyPond version
   is less than the given reference version"
  (deprecate "lilypond-less-than?")
  (< (calculate-version (ly:version))
     (calculate-version ref-version)))

(define-public (lilypond-less-than-or-equal? ref-version)
  "Return #t if the executed LilyPond version
   is less than or equal to the given reference version"
  (deprecate "lilypond-less-than-or-equal?")
  (<= (calculate-version (ly:version))
      (calculate-version ref-version)))

(define-public (lilypond-equals? ref-version)
  "Return #t if the executed LilyPond version
   is equal to the given reference version"
  (deprecate "lilypond-equals?")
  (= (calculate-version (ly:version))
     (calculate-version ref-version)))

(define-public (lilypond-version-string ver-list)
  "Return a string representation of a version list.
    Elements of the list can be either strings or integers"
  (string-join
   (map (lambda (elt)
          (if (integer? elt)
              (number->string elt)
              elt))
     ver-list)
   "."))
