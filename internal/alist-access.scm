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

(define-module (oll-core internal alist-access))
(use-modules
 (lily))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Processing regular alists

;; Create a new empty list with the name <name>.
;; After this call we can rely on the existence
;; and emptyness of this.
(define-public newAlist
   (define-void-function (name)(symbol?)
     "Creates or resets <name> as an empty list."
         (ly:parser-define! name (list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Processing nested alists,
;; called a-trees herein

;; Create a new empty a-tree with the name <name>.
;; This is merely a renamed copy of newAlist as
;; an empty a-tree is actually an empty list.
(define-public newAtree newAlist)

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;% Old functions, to be reviewed
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;% sets one value - replaces the value and leaves the order of elements, if <name> is already present
(define-public setalist
  (define-void-function (parser location alst name val)(symbol? symbol? scheme?)
    (let ((l (ly:parser-lookup alst))
          (setv #t))
      (set! l (map (lambda (p)
                     (if (and (pair? p) (equal? (car p) name))
                         (begin
                          (set! setv #f)
                          (cons name val))
                         p
                         )) l))
      (if setv (set! l (append l (list (cons name val)))))
          (ly:parser-define! alst l)
          )))
;% sets one value - <name> is always placed at the end of the list
(define-public addalist
  (define-void-function (parser location alst name val)
    (symbol? symbol? scheme?)
    (let ((l (ly:parser-lookup alst)))
      (set! l (filter (lambda (p) (and (pair? p)(not (equal? (car p) name)))) l))
     (ly:parser-define! alst (append l (list (cons name val))))
      )))

;% removes one entry from association list
(define-public remalist
  (define-void-function (parser location alst name)(symbol? symbol?)
    (let ((l  (ly:parser-lookup alst)))
          (ly:parser-define! alst
            (filter (lambda (p) (and (pair? p)(not (equal? (car p) name)))) l))
      )))

;% get entry from nested a-list
(define-public (get-a-tree parser location name path)
   (if (not (symbol? name)) (set! name (string->symbol (object->string name))))
   (let ((opts (ly:parser-lookup name)))
     (define (getval ol op)
       (let ((sym (car op)))
         (cond
          ((> (length op) 1)
           (let ((al (assoc-get sym ol #f)))
             (if (list? al)
                 (getval al (cdr op))
                 #f)))
          ((= (length op) 1)
           (assoc-get (car op) ol #f))
          (else #f))))
     (if (list? opts)
         (getval opts path)
         (begin
          (oll:warn location "~A is not list (~A)" name opts)
          #f)
         )))
;% add an entry to a nested a-list
(define (add-a-tree parser location name sympath val assoc-set-append)
   (if (not (symbol? name)) (set! name (string->symbol (object->string name))))
   (let ((opts (ly:parser-lookup name)))
    (define (setval ol op)
      (let ((sym (car op))
            (ol (if (list? ol) ol (begin
                                   (oll:warn location "deleting '~A'" ol)
                                   '()))))
        (if (> (length op) 1)
            (let ((al (assoc-get sym ol '())))
              (if (not (list? al))
                  (begin
                   (oll:warn location "deleting '~A' = '~A'" sym al)
                   (set! al '())
                   ))
              (assoc-set-append ol sym (setval al (cdr op)))
              )
            (let ((ov (assoc-get sym ol #f)))
              ;(if ov (oll:warn location "deleting '~A'" ov))
              (assoc-set-append ol sym val)
              )
            )))
    (set! opts (setval opts sympath)) (ly:parser-define! name opts)
     ))

;% remove an entry from a nested a-list
(define (rem-a-tree parser location name sympath)
  (if (not (symbol? name)) (set! name (string->symbol (object->string name))))
  (let ((opts (ly:parser-lookup name)))
    (define (remval ol op)
      (let ((sym (car op)))
        (if (> (length op) 1)
            (let ((al (assoc-get sym ol '())))
              (set! al (remval al (cdr op)))
              (if (> (length al) 0)
                  (map (lambda (p) (if (and (pair? p)(equal? (car p) sym)) (cons sym al) p)) ol)
                  (filter (lambda (p) (not (and (pair? p)(equal? (car p) sym)))) ol))
              )
            (filter (lambda (p) (not (and (pair? p)(equal? (car p) sym)))) ol)
            )
        ))
    (set! opts (remval opts sympath))
    (ly:parser-define! name opts)
    ))

;% get entry from nested a-list
(define-public getatree
   (define-scheme-function (parser location name sympath)(symbol? list?)
     (get-a-tree parser location name sympath)))
;% add an entry to nested a-list at the end
(define-public addatree
  (define-void-function (parser location name sympath val)(symbol? list? scheme?)
    (add-a-tree parser location name sympath val
      (lambda (l sym val)
        (append (filter (lambda (p) (not (and (pair? p)(equal? (car p) sym)))) l)
          (list (cons sym val)))))))
;% set an entry in nested a-list in place
(define-public setatree
  (define-void-function (parser location name sympath val)(symbol? list? scheme?)
    (add-a-tree parser location name sympath val
      (lambda (l sym val) (assoc-set! l sym val)))))
;% remove an entry from nested a-list
(define-public rematree
  (define-void-function (parser location name sympath)(symbol? list?)
    (rem-a-tree parser location name sympath)))
