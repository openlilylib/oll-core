;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%                                                                             %
;% This file is part of openLilyLib,                                           %
;%                      ===========                                            %
;% the community library project for GNU LilyPond                              %
;% (https://github.com/openlilylib)                                            %
;%              -----------                                                    %
;%                                                                             %
;% Library: oll-core                                                           %
;%          ========                                                           %
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
;%       Copyright Urs Liska 2017                                              %
;%                                                                             %
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define-module (oll-core internal file-handling))
(export
 immediate-include
 read-lines-from-file
 )

(use-modules (lily))
(use-modules (ice-9 rdelim))

;; Immediate inclusion of files
;; Returns #t if file is found and #f if it is missing.
;; If the file is considered to have a language different from nederlands
;; it must be given at the beginning of the file
(define (immediate-include file)
  (if (file-exists? file)
      (let ((parser (ly:parser-clone)))
        (ly:parser-parse-string parser "\\language \"nederlands\"")
        (ly:parser-parse-string parser
          (format "\\include \"~a\"" file))
        #t)
      #f))

;; read a file as a list of lines
(define read-lines-from-file
  (lambda (file)
    (if (file-exists? file)
      (let ((h (open-input-file file))
	    (lines '()))
	(let lp ((line (read-line h 'concat)))
	  (if (eof-object? line)
	      (reverse lines)
	      (begin
		(set! lines (cons line lines))
		(lp (read-line h 'concat))))))
      #f)))

