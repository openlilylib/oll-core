;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

;; This files contains general-purpose predicates for use with LilyPond and openLilylib

(define-module (oll-core internal tools))

(use-modules (srfi srfi-1))

;; String list predicate
(define-public (stringlist? obj)
   "Evaulates to #t when obj is a list containing exclusively of strings."
   (and (list? obj)
        (every string? obj)))

;; convert elements of a string list to a symbol list
(define-public (stringlist->symbol-list obj)
  (map string->symbol obj))

(define-public (string-or-alist? obj)
   "Returns true if obj is a string or a list of pairs (alist)
    (used for mandatory library options)"
   (if (or (string? obj)
           (and (list? obj)
                (every pair? obj)))
       #t #f))

(define-public (symbol-list-or-string? object)
   "Returns true if obj is a symbol list or a string
    (used for arguments passed to os-path functions)."
   (if (or (symbol-list? object)
           (string? object))
       #t #f))




(define-public (symbol-downcase sym)
   (string->symbol (string-downcase (symbol->string sym))))


