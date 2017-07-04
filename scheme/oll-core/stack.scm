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
;%       Copyright Jan-Peter Voigt, Urs Liska, 2016                            %
;%                                                                             %
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define-module (oll-core stack))

(use-modules (oop goops)(lily))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stack

; a stack implementation with methods push, pop and get
(define-class <stack> ()
  (name #:accessor name #:setter set-name! #:init-value "stack")
  (store #:accessor store #:setter set-store! #:init-value '())
  )

; push value on the stack
(define-method (push (stack <stack>) val)
  (set! (store stack) (cons val (store stack))))

; get topmost value from stack without removing it
(define-method (get (stack <stack>))
  (let ((st (store stack)))
    (if (> (length st) 0)
        (car st)
        #f)))

; return and remove topmost value
(define-method (pop (stack <stack>))
  (let ((st (store stack)))
    (if (> (length st) 0)
        (let ((ret (car st)))
          (set! (store stack) (cdr st))
          ret)
        #f)))

; display stack
(define-method (display (stack <stack>) port)
  (for-each (lambda (e)
              (format #t "~A> " (name stack))(display e)(newline)) (store stack)))

; create stack object
(define-public (stack-create)(make <stack>))

; export methods
(export push)
(export get)
(export pop)
(export store)
(export name)
