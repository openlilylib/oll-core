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
;%       Copyright Urs Liska, 2018                                             %
;%                                                                             %
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; This files contains (convenience) tools for handling grobs
;; (in callbacks or engravers)

(define-module (oll-core internal grob-tools))
(use-modules (lily))

(define-public (grob-name grob)
  "Returns the name/type of a grob."
  (assq-ref (ly:grob-property grob 'meta) 'name))

(define-public (filter-grobs-by-name name grob-list)
  "Filters a list of grobs by a grob name/type"
  (filter
   (lambda (grob)
     (eq? (grob-name grob) name))
   grob-list))

(define-public (stem-direction grob)
  "Returns the stem-direction in the current note-column,
    assuming there's a single Stem present in any column
    (even for whole notes or rests)."
  (let*
   ((nc (ly:grob-parent grob X)))
   (if (not (eq? (grob-name nc) 'NoteColumn))
       #f
       (let
        ((stem
          (car
           (filter-grobs-by-name 'Stem
             (ly:grob-array->list (ly:grob-object nc 'elements))))))
        (ly:grob-property stem 'direction)))))
